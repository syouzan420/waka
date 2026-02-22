{-# LANGUAGE Arrows #-}

module MainSF(mainSF) where

import FRP.Yampa (integral,returnA,arr,sscan,delay,SF,(^<<),(>>>),(<<<),(^+^))
import System.Random (StdGen)
import Foreign.C.Types (CFloat)
import Data.Vector2 (vector2,Vector2)
import Data.AffineSpace ((.+^))
import Data.Point2 (Point2(..))

import Inputs (Inputs(..))
import WakaData (WakaData(..),ImgType(..),ImgLR(..),ImgDir(..))

mainSF :: StdGen -> WakaData -> SF Inputs WakaData
mainSF rgen wd = proc i -> do
    rec
       let nwd = wd{wdDouble=d,wdPlayerPos=p,wdPlayerImg=im}
       dv <- (+(1::Double)) ^<< integral      -< wdDouble wd 
       d  <- (+10)          ^<< integral      -< dv
       im <- sscan setImgDir (wdPlayerImg wd) -< i
       p  <- sscan setPos (wdPlayerPos wd)    -< i 
--       v1 <- arr setVel -< (i,vector2 0 0)
--       v <- (vector2 0 0 ^+^) ^<< integral -< v1 
--       p <- (wdPlayerPos wd .+^) ^<< integral -< v
    returnA -< nwd 

setPos :: Point2 CFloat -> Inputs -> Point2 CFloat
setPos p@(Point2 x y) i 
  | inpUp i = if y>10 then p .+^ vector2 0 (-1) else p
  | inpDown i = if y<100 then p .+^ vector2 0 1 else p
  | inpLeft i = if x>10 then p .+^ vector2 (-1) 0 else p
  | inpRight i = if x<100 then p .+^ vector2 1 0 else p
  | otherwise = p

setVel :: (Inputs,Vector2 CFloat) -> Vector2 CFloat 
setVel (i,v)
  | inpUp i = v ^+^ vector2 0 (-10)
  | inpDown i = v ^+^ vector2 0 10
  | inpLeft i = v ^+^ vector2 (-10) 0 
  | inpRight i = v ^+^ vector2 10 0 
  | otherwise = v

setImgDir :: ImgType -> Inputs -> ImgType
setImgDir (ImgType dir lr c) i
  | inpUp i = ImgType ImBack (nlr ImBack) nc
  | inpDown i = ImgType ImFront (nlr ImFront) nc
  | inpLeft i = ImgType ImLeft (nlr ImLeft) nc 
  | inpRight i = ImgType ImRight (nlr ImRight) nc
  | otherwise = ImgType dir lr nc
  where nc = if c==10 then 0 else c+1
        nlr dr = if dir==dr && c==10 then changeLR lr else lr 

changeLR :: ImgLR -> ImgLR
changeLR lr = if lr==ImL then ImR else ImL 
