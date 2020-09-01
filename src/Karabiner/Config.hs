module Karabiner.Config where

import Prelude hiding (mod)
import Data.Aeson
import Data.Aeson.Encode.Pretty hiding (Tab)
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.TypeLits as TL
import GHC.TypeLits (ErrorMessage((:$$:)))

import Karabiner.Config.Internal (prettyConfig, stripNulls)

-- | Builds a main function which outputs the given Root as JSON.
mkMain :: Root -> IO ()
mkMain = LC.putStrLn . encodeRoot

-- | Encode a Root to a JSON lazy ByteString.
encodeRoot :: Root -> LC.ByteString
encodeRoot root = encodePretty' prettyConfig root <> LC.pack "\n"

-- | Converts a text literal to an application regex pattern.
litPat :: Text -> Text
litPat t = '^' `T.cons` (T.replace "." "\\." t) `T.snoc` '$'

-- | Represents a key binding of [a] modifiers (e.g. shift, control) and a key code.
-- The 'a' is polymorphic so we can make distinctions between PhysicalModifier
-- and MetaModifier.
data KeyBinding a = KeyBinding [a] KeyCode

-- | Key binding with no modifiers.
singleKey :: KeyCode -> KeyBinding PhysicalModifier
singleKey = KeyBinding []

-- | Helper for a key binding sequence, only slightly prettier than using a list.
-- Later we can refactor this if we need to chain more than two; that will probably
-- require a type class.
(|->) :: KeyBinding a -> KeyBinding a -> [KeyBinding a]
x |-> y = [x, y]

infix 5 |->

class ToKeyBinding a b c | a b -> c where
  (|+|) :: a -> b -> KeyBinding c
  infix 6 |+|

instance ToKeyBinding PhysicalModifier KeyCode PhysicalModifier where
  mod |+| kc = KeyBinding [mod] kc

instance ToKeyBinding [PhysicalModifier] KeyCode PhysicalModifier where
  mods |+| kc = KeyBinding mods kc

instance ToKeyBinding MetaModifier KeyCode MetaModifier where
  mod |+| kc = KeyBinding [mod] kc

instance ToKeyBinding [MetaModifier] KeyCode MetaModifier where
  mods |+| kc = KeyBinding mods kc

-- | Maps first KeyBinding to second KeyBinding
-- We can map physical modifiers to physical modifiers or meta modifiers
-- to physical modifiers, but we can't (or rather, shouldn't) map
-- any modifiers to meta modifiers; these type class instances enforce this.
-- We provide an instance for this via TypeError to provide nice error
-- messages.
class ManipulatorBuilder a b where
  (!>) :: a -> b -> Manipulator
  infix 4 !>

instance AsAnyModifier a
  => ManipulatorBuilder (KeyBinding a) (KeyBinding PhysicalModifier) where
  (KeyBinding fromMods fromK) !> (KeyBinding toMods toK) =
    Manipulator Basic mf [mt] Nothing
    where
    mf = ManipulatorFrom fromK (FromModifiers $ map asAnyModifier fromMods)
    mt = ManipulatorTo   toK   toMods

instance AsAnyModifier a
  => ManipulatorBuilder (KeyBinding a) [KeyBinding PhysicalModifier] where
  (KeyBinding fromMods fromK) !> toBindingSeq =
    Manipulator Basic mf mts Nothing
    where
    mf = ManipulatorFrom fromK (FromModifiers $ map asAnyModifier fromMods)
    mts = flip map toBindingSeq $ \(KeyBinding toMods toK) ->
            ManipulatorTo toK toMods

instance AsAnyModifier a
  => ManipulatorBuilder (KeyBinding a) KeyCode where
  fromKeys !> toKey = fromKeys !> ([] :: [PhysicalModifier]) |+| toKey

instance
  TL.TypeError
    (     'TL.Text "Unsupported ManipulatorBuilder (!>) usage;"
    ':$$: 'TL.Text "'to' binding must use a PhysicalModifier (e.g. RightControl)"
    ':$$: 'TL.Text "not a MetaModifier (e.g. Control)"
    )
  => ManipulatorBuilder a (KeyBinding MetaModifier) where
  (!>) = undefined

-- | Adds 'frontmost_application_if' condition to Manipulator
(?) :: Manipulator -> [Text] -> Manipulator
m ? ts = m { manipulatorConditions = cs }
  where
  c = ManipulatorCondition FrontmostApplicationIf ts
  cs = Just $ c : fromMaybe [] (manipulatorConditions m)

-- | Same as ? except updates a list of Manipulator
(??) :: [Manipulator] -> [Text] -> [Manipulator]
ms ?? ts = map (? ts) ms

-- | Adds 'frontmost_application_unless' condition to Manipulator
(?!) :: Manipulator -> [Text] -> Manipulator
m ?! ts = m { manipulatorConditions = cs }
  where
  c = ManipulatorCondition FrontmostApplicationUnless ts
  cs = Just $ c : fromMaybe [] (manipulatorConditions m)

infix 3 ?!

-- | Same as ?! except updates a list of Manipulator
(??!) :: [Manipulator] -> [Text] -> [Manipulator]
ms ??! ts = map (?! ts) ms

data Root = Root
  { rootTitle :: Text
  , rootRules :: [Rule]
  }

instance ToJSON Root where
  toJSON (Root title rules) =
    object ["title" .= title, "rules" .= rules]

data Rule = Rule
  { ruleDescription :: Text
  , ruleManipulators :: [Manipulator]
  }

instance ToJSON Rule where
  toJSON (Rule d ms) =
    object ["description" .= d, "manipulators" .= ms]

data Manipulator = Manipulator
  { manipulatorType :: ManipulatorType
  , manipulatorFrom :: ManipulatorFrom
  , manipulatorTo :: [ManipulatorTo]
  , manipulatorConditions :: Maybe [ManipulatorCondition]
  }

instance ToJSON Manipulator where
  toJSON (Manipulator typ from to conds) = object $ stripNulls
    [ "type" .= typ
    , "from" .= from
    , "to" .= to
    , "conditions" .= conds
    ]

data ManipulatorType = Basic

manipulatorTypeToText :: ManipulatorType -> Text
manipulatorTypeToText = \case
  Basic -> "basic"

instance ToJSON ManipulatorType where
  toJSON = toJSON . manipulatorTypeToText

data ManipulatorFrom = ManipulatorFrom
  { fromKeyCode :: KeyCode
  , fromModifiers :: FromModifiers
  }

instance ToJSON ManipulatorFrom where
  toJSON (ManipulatorFrom k ms) =
    object ["key_code" .= k, "modifiers" .= ms]

data ManipulatorTo = ManipulatorTo
  { toKeyCode :: KeyCode
  , toModifiers :: [PhysicalModifier]
  }

instance ToJSON ManipulatorTo where
  toJSON (ManipulatorTo k ms) =
    object ["key_code" .= k, "modifiers" .= ms]

data ManipulatorCondition = ManipulatorCondition
  { conditionType :: ManipulatorConditionType
  , conditionBundleIdentifiers :: [Text]
  }

instance ToJSON ManipulatorCondition where
  toJSON (ManipulatorCondition t bis) =
    object ["type" .= t, "bundle_identifiers" .= bis]

data ManipulatorConditionType
  = FrontmostApplicationUnless
  | FrontmostApplicationIf

manipulatorConditionTypeToText :: ManipulatorConditionType -> Text
manipulatorConditionTypeToText = \case
  FrontmostApplicationUnless -> "frontmost_application_unless"
  FrontmostApplicationIf -> "frontmost_application_if"

instance ToJSON ManipulatorConditionType where
  toJSON = toJSON . manipulatorConditionTypeToText

data FromModifiers = FromModifiers
  { modifiersMandatory :: [AnyModifier]
  }

instance ToJSON FromModifiers where
  toJSON (FromModifiers m) =
    object ["mandatory" .= m]

data PhysicalModifier
  = LeftShift | RightShift
  | LeftControl | RightControl
  | LeftOption | RightOption
  | LeftCommand | RightCommand

instance ToJSON PhysicalModifier where
  toJSON = \case
    LeftShift -> "left_shift"
    RightShift -> "right_shift"
    LeftControl -> "left_control"
    RightControl -> "right_control"
    LeftOption -> "left_option"
    RightOption -> "right_option"
    LeftCommand -> "left_command"
    RightCommand -> "right_command"

data MetaModifier
  = Shift
  | Control
  | Option
  | Command

instance ToJSON MetaModifier where
  toJSON = \case
    Shift -> "shift"
    Control -> "control"
    Option -> "option"
    Command -> "command"

data AnyModifier
  = ModifierFromPhysical PhysicalModifier
  | ModifierFromMeta MetaModifier

class AsAnyModifier a where
  asAnyModifier :: a -> AnyModifier

instance AsAnyModifier PhysicalModifier where
  asAnyModifier = ModifierFromPhysical

instance AsAnyModifier MetaModifier where
  asAnyModifier = ModifierFromMeta

instance ToJSON AnyModifier where
  toJSON mf = case mf of
    ModifierFromPhysical m -> toJSON m
    ModifierFromMeta m -> toJSON m

-- | Key codes available for binding
-- The full list can be viewed in the Karabiner-Elements source
-- https://github.com/tekezo/Karabiner-Elements/blob/master/src/share/types.hpp
data KeyCode
  = A | B | C | D | E | F | G | H | I | J | K | L | M
  | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Zero
  | Spacebar | Backspace
  | OpenBracket | CloseBracket
  | RightArrow | LeftArrow | UpArrow | DownArrow
  | ReturnOrEnter
  | Tab
  | Comma | Semicolon

-- Number keys on the keyboard, useful for generating key maps via loops.
numbers :: [KeyCode]
numbers = [One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Zero]

keyCodeToString :: IsString a => KeyCode -> a
keyCodeToString = \case
  A -> "a"
  B -> "b"
  C -> "c"
  D -> "d"
  E -> "e"
  F -> "f"
  G -> "g"
  H -> "h"
  I -> "i"
  J -> "j"
  K -> "k"
  L -> "l"
  M -> "m"
  N -> "n"
  O -> "o"
  P -> "p"
  Q -> "q"
  R -> "r"
  S -> "s"
  T -> "t"
  U -> "u"
  V -> "v"
  W -> "w"
  X -> "x"
  Y -> "y"
  Z -> "z"
  One -> "1"
  Two -> "2"
  Three -> "3"
  Four -> "4"
  Five -> "5"
  Six -> "6"
  Seven -> "7"
  Eight -> "8"
  Nine -> "9"
  Zero -> "0"
  Spacebar -> "spacebar"
  Backspace -> "delete_or_backspace"
  OpenBracket -> "open_bracket"
  CloseBracket -> "close_bracket"
  RightArrow -> "right_arrow"
  LeftArrow -> "left_arrow"
  DownArrow -> "down_arrow"
  UpArrow -> "up_arrow"
  ReturnOrEnter -> "return_or_enter"
  Tab -> "tab"
  Comma -> "comma"
  Semicolon -> "semicolon"

instance ToJSON KeyCode where
  toJSON = keyCodeToString
