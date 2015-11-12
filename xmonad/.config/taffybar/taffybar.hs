import System.Taffybar

import System.Taffybar.Systray
import System.Taffybar.Pager
import System.Taffybar.SimpleClock
import System.Taffybar.MPRIS
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.WorkspaceSwitcher

main = do
  let clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M</span>" 1
      wss = wspaceSwitcherNew =<< pagerNew defaultPagerConfig
      note = notifyAreaNew defaultNotificationConfig
      mpris = mprisNew defaultMPRISConfig
      tray = systrayNew
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ wss, note ]
                                        , endWidgets = [ tray, clock, mpris ]
                                        , barPosition = Bottom
                                        , barHeight = 20
                                        }
