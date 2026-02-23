module SDLDraw (draw) where

import SDL (V4(V4),V2(V2),Point(P))
import SDL.Primitive (fillCircle)
import SDL.Video.Renderer (copy,Rectangle(..))
import Data.Point2(Point2(..))
import WakaData (getSpriteName,WakaData(..),FieldData(..))

draw :: WakaData -> IO ()
draw wd = do 
  let renderer = wdRenderer wd
  let getSprite = wdGetSprite wd
  let fd = wdFieldData wd
  let (Point2 px py) = fdPlayerPos fd
  let imgType = fdPlayerImg fd
  let plSpriteName = getSpriteName imgType
  let pli = getSprite plSpriteName 
  let i = wdDouble wd
  fillCircle renderer (V2 60 (10+floor i)) 10 (V4 102 178 255 255)
  copy renderer pli 
        Nothing (Just (Rectangle (P (V2 (floor px) (floor py))) (V2 16 16)))
  putStrLn (show i ++" "++ show px ++ "," ++ show py)

