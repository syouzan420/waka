{-# LANGUAGE Arrows #-}

module MainSF(mainSF) where

import FRP.Yampa (integral,returnA,arr,sscan,SF,(^<<),(>>>),(^+^))
import System.Random (StdGen)
import Foreign.C.Types (CFloat)
import Data.Vector2 (vector2,Vector2)
import Data.AffineSpace ((.+^))

import Inputs (Inputs(..))
import WakaData (WakaData(..),ImgType(..),ImgLR(..),ImgDir(..))

mainSF :: StdGen -> WakaData -> SF Inputs WakaData
mainSF rgen wd = proc i -> do
    rec
       let nwd = wd{wdDouble=d,wdPlayerPos=p,wdPlayerImg=im}
       dv <- (+(1::Double)) ^<< integral -< wdDouble wd 
       d <- (+10) ^<< integral -< dv
       v1 <- arr setVel -< (i,vector2 0 0)
       im <- sscan setImgDir (wdPlayerImg wd) -< i
       v <- (vector2 0 0 ^+^) ^<< integral -< v1 
       p <- (wdPlayerPos wd .+^) ^<< integral -< v
    returnA -< nwd 

setVel :: (Inputs,Vector2 CFloat) -> Vector2 CFloat 
setVel (i,v)
  | inpUp i = v ^+^ vector2 0 (-10)
  | inpDown i = v ^+^ vector2 0 10
  | inpLeft i = v ^+^ vector2 (-10) 0 
  | inpRight i = v ^+^ vector2 10 0 
  | otherwise = v

setImgDir :: ImgType -> Inputs -> ImgType
setImgDir (ImgType dir lr) i
  | inpUp i = ImgType ImBack (if dir==ImBack then changeLR lr else ImL)
  | inpDown i = ImgType ImFront (if dir==ImFront then changeLR lr else ImL) 
  | inpLeft i = ImgType ImLeft (if dir==ImLeft then changeLR lr else ImL) 
  | inpRight i = ImgType ImRight (if dir==ImRight then changeLR lr else ImL)
  | otherwise = ImgType dir lr

changeLR :: ImgLR -> ImgLR
changeLR lr = if lr==ImL then ImR else ImL 
