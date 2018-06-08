import Control.Applicative ((<*))
import qualified Data.Map as M
import Data.Monoid ((<>))

import XMonad
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts, docksEventHook, ToggleStruts(..))
import XMonad.Layout.MultiToggle (mkToggle1, Toggle(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(..))
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP, additionalKeys)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Cursor (setDefaultCursor)

type KeyMap = M.Map (KeyMask, KeySym) (X ())

data Mode = Normal | Presentation

main = xmonad . ewmh $ myConfig

myConfig = conf { modMask = mod
                , terminal = term Normal
                , focusedBorderColor = "#859900"
                , layoutHook = layout
                , manageHook = manageDocks
                , handleEventHook = docksEventHook
                , startupHook = --Spawnonce "taffybar" <>
                                setDefaultCursor xC_heart
                , workspaces = workspaceNames
                }
           `additionalKeysP` (
             [ -- fullscreen
               ("M-e", sendMessage $ Toggle NBFULL)
                 -- i3-like keybindings, because I’m spoiled
             , ("M-S-x", kill)
                 -- exchange M-Ret and M-S-Ret
             , ("M-<Return>", spawn $ term Normal)
             , ("C-M-<Return>", spawn $ term Presentation)
             , ("M-S-<Return>", windows W.swapMaster)
             -- toogle toolbar(s)
             , ("M-b", sendMessage ToggleStruts)
             -- open simple exec dmenu
             ]
             ++
             -- something something workspaces
             [ (otherModMasks ++ "M-" ++ [key], action tag)
               | (tag, key)  <- zip workspaceNames "123456789"
               , (otherModMasks, action) <- [ ("", windows . W.greedyView)
                                            , ("S-", windows . W.shift)]
             ]
             ++
             -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
             -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
             [ ("M-v", focusToScreen 0)
             , ("M-l", focusToScreen 1)
             , ("M-c", focusToScreen 2)
             , ("M-S-v", windowToScreen 0)
             , ("M-S-l", windowToScreen 1)
             , ("M-S-c", windowToScreen 2)
             ]
             -- ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
             --   | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
             --    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
           )
           `additionalKeys`
           -- arrow keys should move as well (hjkl blindness)
           [ ((mod, xK_Up), windows W.focusUp)
           , ((mod, xK_Down), windows W.focusDown) ]
           `removeKeysP`
             [
               -- previous kill command
               "M-S-c"
               -- It is way to easy to kill everything by default
             , "M-S-q"
               -- no idea, I want to use it for Mozc
             , "M-n"
             ]
  where
    conf = def
    workspaceNames = workspaces conf
    mod = mod4Mask
    -- TODO: meh
    term :: Mode -> String
    term Normal = "lilyterm"
    term Presentation = "lilyterm -u ~/.config/lilyterm/pres.conf"

    toScreen with number = screenWorkspace 0 >>= \ws -> whenJust ws (windows . with)
    focusToScreen = toScreen W.view
    windowToScreen = toScreen W.shift

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
