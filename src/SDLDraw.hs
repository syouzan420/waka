module SDLDraw (draw) where

import SDL (V4(V4),V2(V2),Point(P))
import SDL.Primitive (fillCircle)
import SDL.Video.Renderer (copy,Rectangle(..))

import SpriteName (SpriteName(..))
import WakaData (WakaData(..))

draw :: WakaData -> IO ()
draw wd = do 
  let renderer = wdRenderer wd
  let getSprite = wdGetSprite wd
  let pli = getSprite PlayerLeft
  let i = wdDouble wd
  fillCircle renderer (V2 60 (10+fromIntegral(floor i))) 10 (V4 102 178 255 255)
  copy renderer pli Nothing (Just (Rectangle (P (V2 10 10)) (V2 16 16)))
  putStrLn ("Hello " ++ show i)
