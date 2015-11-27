import Control.Applicative ((<*))
import qualified Data.Map as M
import Data.Monoid ((<>))

import XMonad
import XMonad.Actions.UpdatePointer (updatePointer, PointerPosition(..))
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts, docksEventHook, ToggleStruts(..))
import XMonad.Layout.MultiToggle (mkToggle1, Toggle(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(..))
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP, additionalKeys)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Cursor (setDefaultCursor)

type KeyMap = M.Map (KeyMask, KeySym) (X ())

main = xmonad . ewmh $ myConfig

myConfig = conf { modMask = mod
                , terminal = term
                , focusedBorderColor = "#859900"
                , layoutHook = layout
                , logHook = updatePointer $ Nearest
                , manageHook = manageDocks
                , handleEventHook = docksEventHook
                , startupHook = spawnOnce "taffybar"
                                <> setDefaultCursor xC_heart
                , workspaces = workspaceNames
                }
           `additionalKeysP` (
             [ ("M-e", sendMessage $ Toggle NBFULL)
                 -- i3-like keybindings, because I’m spoiled
             , ("M-S-x", kill)
                 -- exchange M-Ret and M-S-Ret
             , ("M-<Return>", spawn term)
             , ("M-S-<Return>", windows W.swapMaster)
                 -- toogle toolbar(s)
             , ("M-b", sendMessage ToggleStruts)
             ]
             ++
             [ (otherModMasks ++ "M-" ++ [key], action tag)
               | (tag, key)  <- zip workspaceNames "123456789"
               , (otherModMasks, action) <- [ ("", windows . W.view) -- was W.greedyView
                                            , ("S-", windows . W.shift)]
             ]
           )
           `removeKeysP`
             [
               -- previous kill command
               "M-S-c"
               -- It is way to easy to kill everything by default
             , "M-S-q"
             ]
  where
    conf = defaultConfig
    workspaceNames = workspaces conf
    mod = mod4Mask
    -- TODO: meh
    term = "lilyterm -x fish"

-- copied from Xmonad.Config
layout = avoidStruts $ toggleFullscreen $ tiled ||| Mirror tiled
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
     toggleFullscreen = mkToggle1 NBFULL
