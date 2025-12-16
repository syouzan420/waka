module WithSDLVideo (withSDLVideo,withRenderer,draw) where

import Control.Monad.IO.Class (MonadIO)
import Foreign.C.Types (CInt,CFloat)
import SDL (($=),V4(V4),V2(V2),cursorVisible)
import SDL.Primitive (fillCircle)
import SDL.Video (createWindow,defaultWindow,windowInitialSize,createRenderer
                 ,defaultRenderer,destroyWindow
                 )
import SDL.Video.Renderer (rendererScale,rendererDrawBlendMode,rendererDrawColor
                          ,clear,present,rendererType
                          ,Renderer
                          ,BlendMode(BlendAlphaBlend)
                          ,RendererType(AcceleratedVSyncRenderer))

import WakaData (windowSize,title,WakaData(wdRenderer,wdDouble))

screenScale :: V2 CFloat
screenScale = V2 4 4

windowDimensions :: V2 CInt
windowDimensions = fmap round $ (*) <$> screenScale <*> windowSize

withSDLVideo :: MonadIO m => (Renderer -> m a) -> m ()
withSDLVideo op = do
  window <- createWindow title (defaultWindow {windowInitialSize = windowDimensions})
  renderer <- createRenderer window (-1) defaultRenderer {rendererType = AcceleratedVSyncRenderer}
  rendererScale renderer $= screenScale
  rendererDrawBlendMode renderer $= BlendAlphaBlend
  cursorVisible $= False
  _ <- op renderer
  destroyWindow window

withRenderer :: MonadIO m => Renderer -> m a -> m ()
withRenderer renderer op = do 
  rendererDrawColor renderer $= V4 0 0 0 0
  clear renderer
  _ <- op
  present renderer

draw :: WakaData -> IO ()
draw wd = do 
  let renderer = wdRenderer wd
  let i = wdDouble wd
  fillCircle renderer (V2 60 (10+fromIntegral(floor i))) 10 (V4 102 178 255 255)
  putStrLn ("Hello " ++ show i)
