{-# LANGUAGE Arrows #-}

module MainSF(mainSF) where

import FRP.Yampa (integral,returnA,arr,SF,(^<<),(>>>),(^+^))
import System.Random (StdGen)
import Foreign.C.Types (CFloat)
import Data.Point2 (Point2(..))
import Data.Vector2 (vector2,Vector2)
import Data.AffineSpace ((.+^))

import Inputs (Inputs(..))
import WakaData (WakaData(..))

mainSF :: StdGen -> WakaData -> SF Inputs WakaData
mainSF rgen wd = proc i -> do
    rec
       let nwd = wd{wdDouble=d,wdPlayerPos=p}
       dv <- (+(1::Double)) ^<< integral -< wdDouble wd 
       d <- (+10) ^<< integral -< dv
       v1 <- arr setVel -< (i,vector2 0 0)
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
