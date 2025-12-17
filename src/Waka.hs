module Waka where

import Control.Monad (when)
import System.Random (getStdGen)
import System.Exit (exitSuccess)
import Data.IORef (IORef)
import FRP.Yampa (reactimate)
import SDL.Event (pollEvents,eventPayload,EventPayload(..))
import SDL.Input.Keyboard (getKeyboardState)

import WithSDL (withSDL)
import WithSDLAudio (withSDLAudio)
import WithSDLVideo (withSDLVideo)
import WithIORef (withIORefInit,getTimeDifference)
import IOAction (ioact)
import Inputs (initInput,readInputs,Inputs(inpQ))
import MainSF (mainSF)
import WakaData (loadWakaData,WakaData)

wakaMain :: IO ()
wakaMain = withSDL $ do 
    withSDLAudio $ do
      withSDLVideo $ \renderer -> do
        wd <- loadWakaData renderer
        withIORefInit $ \tRef -> do
          rgen <- getStdGen
          reactimate (return initInput)
                     (input tRef)
                     output
                     (mainSF rgen wd)

input :: IORef Double -> Bool -> IO (Double,Maybe Inputs) 
input tRef _ = do
  es <- pollEvents
  keyDown <- getKeyboardState
  let inputs = readInputs keyDown 
  dt <- getTimeDifference tRef 
  when (any (isQuit . eventPayload) es || inpQ inputs) exitSuccess
  return (dt,Just inputs) 

isQuit :: EventPayload -> Bool
isQuit QuitEvent = True
isQuit (WindowClosedEvent _) = True
isQuit _ = False
  
output ::  Bool -> WakaData -> IO Bool
output _ wd = ioact wd >> return False
