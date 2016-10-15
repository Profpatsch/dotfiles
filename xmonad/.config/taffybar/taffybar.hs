{-# LANGUAGE FlexibleInstances #-}
import System.Taffybar

import System.Taffybar.Battery
import System.Taffybar.Widgets.PollingBar (barColor)
import System.Taffybar.Systray
import System.Taffybar.Pager
import System.Taffybar.SimpleClock
import System.Taffybar.MPRIS
-- import System.Taffybar.FreedesktopNotifications
import System.Taffybar.WorkspaceSwitcher

main = do
  let clock = textClockNew Nothing ("<span fgcolor='" ++ cs2s Brown ++ "'>%a %b %_d </span><span fgcolor='" ++ cs2s Yellow ++ "'>%H:%M</span>") 1
      wss = wspaceSwitcherNew =<< pagerNew defaultPagerConfig
      -- note = notifyAreaNew defaultNotificationConfig
      mpris = mprisNew defaultMPRISConfig
      tray = systrayNew
      bat = batteryBarNew $ defaultBatteryConfig { barColor = cs2d.myColor }
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ wss{-, note-} ]
                                        , endWidgets = [ tray, clock, bat ]
                                        , barPosition = Bottom
                                        , barHeight = 20
                                        }
  where
    myColor charge
      | charge >= 0.4 = Green
      | charge >= 0.2 = Yellow
      | otherwise     = Red

data Colorscheme = Red
                | Green
                | Yellow
                | Brown

cs2s :: Colorscheme -> String
cs2s Red    = "red"
cs2s Green  = "limegreen"
cs2s Yellow = "gold"
cs2s Brown  = "brown"

cs2d :: Colorscheme -> (Double, Double, Double)
cs2d Red    = (1,0,0)
cs2d Green  = (0.196, 0.803, 0.196)
cs2d Yellow = (1,     0.843, 0)
cs2d Brown  = (0.647, 0.164, 0.164)

-- getLoad :: IO String
-- getLoad = do
--   f <- readFile "/proc/loadavg"
--   words f

foo = "baz"
