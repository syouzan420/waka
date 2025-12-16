module WithSDL (withSDL) where

import SDL (quit)
import SDL.Init (initializeAll)
import qualified SDL.Font as F
import qualified SDL.Image as I
import Control.Monad.IO.Class (MonadIO)

withSDL :: MonadIO m => m a -> m ()
withSDL op = do
  initializeAll
  F.initialize
  I.initialize []
  _ <- op
  quit
