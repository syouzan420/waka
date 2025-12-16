module WithSDLAudio(withSDLAudio) where

import SDL.Mixer (openAudio,closeAudio,quit
                 ,Audio(..),Format(FormatS16_Sys),Output(Stereo))
import Control.Monad.IO.Class (MonadIO)

desiredAudioSpec :: Audio
desiredAudioSpec = Audio {
    audioFrequency = 44100
   ,audioFormat = FormatS16_Sys
   ,audioOutput = Stereo
}

withSDLAudio :: MonadIO m => m a -> m ()
withSDLAudio op = do
  openAudio desiredAudioSpec 1024
  _ <- op
  closeAudio
  quit
