module IOAction(ioact) where

import WithSDLVideo (withRenderer,draw)
import WakaData (WakaData(..))

ioact :: WakaData -> IO ()
ioact wd = do
  let renderer = wdRenderer wd
  withRenderer renderer $ draw wd
