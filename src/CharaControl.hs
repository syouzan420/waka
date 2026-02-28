module CharaControl(playerMove) where

import Foreign.C.Types (CFloat,CInt)
import Data.Vector2 (vector2)
import Data.AffineSpace ((.+^))
import Data.Point2 (Point2(..))

import Inputs (Inputs(..))
import WakaData (ImgType(..),ImgLR(..),ImgDir(..),FieldData(..)
                ,InputMode(..))

playerMove :: FieldData -> (Inputs,InputMode) -> FieldData
playerMove fd@(FieldData p im) (i,IField) = 
      fd{fdPlayerPos=setPos p i, fdPlayerImg=setImgDir im i} 
playerMove fd _ = fd

setPos :: Point2 CFloat -> Inputs -> Point2 CFloat
setPos p@(Point2 x y) i 
  | inpUp i = if y>10 then p .+^ vector2 0 (-1) else p
  | inpDown i = if y<100 then p .+^ vector2 0 1 else p
  | inpLeft i = if x>10 then p .+^ vector2 (-1) 0 else p
  | inpRight i = if x<100 then p .+^ vector2 1 0 else p
  | otherwise = p

setImgDir :: (ImgType,CInt) -> Inputs -> (ImgType,CInt)
setImgDir (ImgType chara dir lr,c) i
  | inpUp i = (ImgType chara ImBack (nlr ImBack), nc)
  | inpDown i = (ImgType chara ImFront (nlr ImFront), nc)
  | inpLeft i = (ImgType chara ImLeft (nlr ImLeft), nc) 
  | inpRight i = (ImgType chara ImRight (nlr ImRight), nc)
  | otherwise = (ImgType chara dir lr, nc)
  where nc = if c==10 then 0 else c+1
        nlr dr = if dir==dr && c==10 then changeLR lr else lr 

changeLR :: ImgLR -> ImgLR
changeLR lr = if lr==ImL then ImR else ImL 
