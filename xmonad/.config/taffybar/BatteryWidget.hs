module BatteryBar where

import System.Taffybar.Widgets.PollingBar (barColor)

-- | A fancy graphical battery widget that represents the current
-- charge as a colored vertical bar.  There is also a textual
-- percentage readout next to the bar.
batteryBarNew :: BarConfig -- ^ Configuration options for the bar display
                 -> Double -- ^ Polling period in seconds
                 -> IO Widget
batteryBarNew battCfg pollSeconds = do
  battCtxt <- batteryContextNew
  case battCtxt of
    Nothing -> do
      let lbl :: Maybe String
          lbl = Just "No battery"
      labelNew lbl >>= return . toWidget
    Just ctxt -> do
      -- This is currently pretty inefficient - each poll period it
      -- queries the battery twice (once for the label and once for
      -- the bar).
      --
      -- Converting it to combine the two shouldn't be hard.
      b <- hBoxNew False 1
           txt <- textBatteryNew   "$percentage$%" pollSeconds
      r <- newIORef ctxt
      bar <- pollingBarNew battCfg pollSeconds (battPct r)
      boxPackStart b bar PackNatural 0
      boxPackStart b txt PackNatural 0
      widgetShowAll b
      return (toWidget b)
