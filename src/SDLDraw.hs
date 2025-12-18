module SDLDraw (draw) where

import SDL (V4(V4),V2(V2),Point(P))
import SDL.Primitive (fillCircle)
import SDL.Video.Renderer (copy,Rectangle(..))
import Data.Point2(Point2(..))

import SpriteName (SpriteName(..))
import WakaData (WakaData(..))

draw :: WakaData -> IO ()
draw wd = do 
  let renderer = wdRenderer wd
  let getSprite = wdGetSprite wd
  let pli = getSprite PlayerFrontLeft
  let i = wdDouble wd
  let (Point2 px py) = wdPlayerPos wd
  let px' = fromIntegral $ floor px
  let py' = fromIntegral $ floor py
  fillCircle renderer (V2 60 (10+fromIntegral(floor i))) 10 (V4 102 178 255 255)
  copy renderer pli Nothing (Just (Rectangle (P (V2 px' py')) (V2 16 16)))
  putStrLn ("Hello " ++ show i)
