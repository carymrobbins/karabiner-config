module Karabiner.Config.Internal where

import Data.Aeson.Types
import Data.Text (Text)
import Data.Aeson.Encode.Pretty hiding (Tab)
import Data.Functor ((<&>))
import System.Directory (getHomeDirectory)

expandUser :: FilePath -> IO FilePath
expandUser = \case
  '~':'/':path -> getHomeDirectory <&> (<> ('/':path))
  path -> pure path

prettyConfig :: Config
prettyConfig = defConfig
  { confIndent = Spaces 2
  , confCompare = prettyConfigCompare
  }

prettyConfigCompare :: Text -> Text -> Ordering
prettyConfigCompare = keyOrder
  [ "title"
  , "rules"
  , "description"
  , "manipulators"
  , "type"
  , "from"
  , "to"
  , "conditions"
  , "key_code"
  , "modifiers"
  ]

stripNulls :: [Pair] -> [Pair]
stripNulls xs = filter (\(_,v) -> v /= Null) xs
