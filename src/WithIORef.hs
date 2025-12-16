module WithIORef(withIORefInit,getTimeDifference) where

import Data.Time.Clock.System (getSystemTime,SystemTime(..))
import Data.IORef (IORef,newIORef,readIORef,writeIORef)
import Control.Monad.IO.Class (MonadIO,liftIO)

floatSeconds :: SystemTime -> Double
floatSeconds tS = fromIntegral (systemSeconds tS) + fromIntegral (systemNanoseconds tS) / 1000000000

withIORefInit :: MonadIO m => (IORef Double -> m a) -> m ()
withIORefInit op = do
  tS <- liftIO getSystemTime
  let seconds = floatSeconds tS
  tRef <- liftIO $ newIORef seconds
  _ <- op tRef
  return ()

getTimeDifference :: IORef Double -> IO Double
getTimeDifference tRef = do
  seconds <- readIORef tRef
  tS <- getSystemTime
  let seconds' = floatSeconds tS
      dt = min 0.016 (seconds' - seconds)
  writeIORef tRef seconds'
  return dt 
