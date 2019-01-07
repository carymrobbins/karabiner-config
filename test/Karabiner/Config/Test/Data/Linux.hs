module Karabiner.Config.Test.Data.Linux where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

import Karabiner.Config

expectedJSON :: ByteString
expectedJSON = $(embedFile "test/data/linux.json")

root :: Root
root = Root "Linux Compat" [rule]
  where
  rule = Rule "Various remappings to make it feel like linux" $ mempty
    <> generalRemaps
    <> terminalRemaps
    <> intellijRemaps
    <> slackRemaps
    <> notTerminalOrIntelliJRemaps
    <> skhdRemaps
    <> chunkwmRemaps

  generalRemaps =
    [ -- Spotlight
      Command |+| P !> RightCommand |+| Spacebar
    ]

  terminalRemaps = concat
    [
      -- Tmux: Switch windows with Ctrl+<number>
      numbers >>= \n -> [ Option |+| n !> tmuxPrefix |-> singleKey n ]
    , [
        -- Tmux: Next window
        Option |+| N !> tmuxPrefix |-> singleKey N
        -- Tmux: Previous window
      , Option |+| P !> tmuxPrefix |-> singleKey P
        -- Copy
      , [Control, Shift] |+| C !> RightCommand |+| C
        -- Paste
      , [Control, Shift] |+| V !> RightCommand |+| V
      ]
    ] ?? [iterm]

  -- The Option + Key mappings in IntelliJ don't work consistently due
  -- to Mac's preference to insert unicode chars instead.
  -- These mappings override that behavior.
  intellijRemaps =
    [ -- Jump to Super method
      Option |+| U !> RightCommand |+| U
      -- Preferences
    , [Control, Option] |+| S !> RightCommand |+| Comma
      -- Project Structure
    , [Shift, Control, Option] |+| S !> RightCommand |+| Semicolon
    ] ?? [intellij]

  slackRemaps =
    (numbers >>= \n -> [ Control |+| n !> RightCommand |+| n ])
    ?? [slack]

  -- These bindings don't work well with iterm2 or intellij due
  -- to clashes with vim, tmux, etc, so excluding them with ??!
  notTerminalOrIntelliJRemaps =
    (
      -- Useful for moving to tab n of an app, e.g. browser.
      (map (\n -> Option |+| n !> RightCommand |+| n) numbers)
      <>
      [ -- New tab
        Control |+| T !> RightCommand |+| T
        -- Open closed tab
      , [Control, Shift] |+| T !> [RightCommand, RightShift] |+| T
        -- Close tab
      , Control |+| W !> RightCommand |+| W
        -- Copy
      , Control |+| C !> RightCommand |+| C
        -- Cut
      , Control |+| X !> RightCommand |+| X
        -- Paste
      , Control |+| V !> RightCommand |+| V
        -- Paste without formatting
      , [Control, Shift] |+| V !> [RightCommand, RightShift] |+| V
        -- Select all
      , Control |+| A !> RightCommand |+| A
        -- Find
      , Control |+| F !> RightCommand |+| F
        -- Reload
      , Control |+| R !> RightCommand |+| R
        -- Force reload
      , [Shift, Control] |+| R !> [RightShift, RightCommand] |+| R
        -- Undo
      , Control |+| Z !> RightCommand |+| Z
        -- Redo
      , Control |+| Y !> RightCommand |+| Y
        -- Highlight address
      , Control |+| L !> RightCommand |+| L
        -- Previous tab
      , [Option, Shift] |+| OpenBracket  !> [RightCommand, RightShift] |+| OpenBracket
        -- Next tab
      , [Option, Shift] |+| CloseBracket !> [RightCommand, RightShift] |+| CloseBracket
        -- Back / Forward
      , Option |+| LeftArrow  !> RightCommand |+| LeftArrow
      , Option |+| RightArrow !> RightCommand |+| RightArrow
        -- Delete previous word
      , Control |+| Backspace !> RightOption |+| Backspace
        -- Chrome: Private tab
      , [Control, Shift] |+| N !> [RightCommand, RightShift] |+| N
        -- Firefox: Private tab
      , [Control, Shift] |+| P !> [RightCommand, RightShift] |+| P
        -- Slack: Go to conversation
      , Control |+| K !> RightCommand |+| K
      , Control |+| ReturnOrEnter !> RightCommand |+| ReturnOrEnter
      ]
    ) ??! [iterm, intellij]

  skhdRemaps = [One, Two, Three, Four, Five] >>= \n ->
    [ -- Mapped in ~/.skhdrc
      Command |+| n !> chunkMod |+| n
    ]

  -- Key maps to use simulate Xmonad.
  chunkwmRemaps =
    [
      -- Focus monitor 1
      Command |+| W !> chunkMod |+| W
      -- Focus monitor 2
    , Command |+| E !> chunkMod |+| E
      -- Focus monitor 3
    , Command |+| R !> chunkMod |+| R
      -- Move window to monitor 1
    , [Shift, Command] |+| W !> chunkShiftMod |+| W
      -- Move window to monitor 2
    , [Shift, Command] |+| E !> chunkShiftMod |+| E
      -- Move window to monitor 3
    , [Shift, Command] |+| R !> chunkShiftMod |+| R
      -- Focus previous window
    , Command |+| J !> chunkMod |+| J
      -- Focus next window
    , Command |+| K !> chunkMod |+| K
      -- Move window to previous
    , [Shift, Command] |+| J !> chunkShiftMod |+| J
      -- Move window to next
    , [Shift, Command] |+| K !> chunkShiftMod |+| K
      -- Default (bsp) layout
    , Command |+| Spacebar !> chunkMod |+| Spacebar
      -- Full (monocle) layout
    , [Shift, Command] |+| Spacebar !> chunkShiftMod |+| Spacebar
      -- Toggle float current window
    , Command |+| T !> chunkMod |+| T
      -- Grow region
    , Command |+| H !> chunkMod |+| H
      -- Shrink region
    , Command |+| L !> chunkMod |+| L
    ]

  -- My ~/.chunkwmrc uses these keys as "mod" keys
  chunkMod = [RightControl, RightOption, RightCommand]
  chunkShiftMod = RightShift : chunkMod

  tmuxPrefix = RightControl |+| A

  -- Patterns for binding keys only for certain apps.
  iterm = litPat "com.googlecode.iterm2"
  -- chrome = litPat "com.google.Chrome"
  intellij = litPat "com.jetbrains.intellij"
  slack = litPat "com.tinyspeck.slackmacgap"
