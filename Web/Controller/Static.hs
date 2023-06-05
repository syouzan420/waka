module Web.Controller.Static where
import Web.Controller.Prelude
import Web.View.Static.Welcome
import Web.View.Static.Code
import Control.Concurrent.Timer (repeatedTimer)
import Control.Concurrent.Suspend (msDelay)
import Data.IORef (newIORef,readIORef,writeIORef)


instance Controller StaticController where
    action WelcomeAction = render WelcomeView
    action CodeAction {arg} = render CodeView {arg}
    action TimerAction = do
      state <- newIORef 0
      _ <- repeatedTimer (timerAction state) (msDelay 1000)
      return ()
      

timerAction :: IORef Int -> IO ()
timerAction state = do
  i <- readIORef state 
  putStrLn (show i) 
  writeIORef state (i+1)

