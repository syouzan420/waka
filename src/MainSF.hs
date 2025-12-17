{-# LANGUAGE Arrows #-}

module MainSF(mainSF) where

import FRP.Yampa (integral,returnA,arr,SF,(^<<),(>>>),(^+^))
import System.Random (StdGen)

import Inputs (Inputs(..))
import WakaData (WakaData(..))

mainSF :: StdGen -> WakaData -> SF Inputs WakaData
mainSF rgen wd = proc i -> do
    rec
       let nwd = wd{wdDouble=p}
--       p <- (+(1::Double)) ^<< integral -< wdDouble nwd 
       v1 <- arr setVel -< (i,0)
       v <- (1 ^+^) ^<< integral -< v1 
       p <- (10 ^+^) ^<< integral -< v
    returnA -< nwd 

setVel :: (Inputs,Double) -> Double
setVel (i,v)
  | inpUp i = v - 10 
  | inpDown i = v + 10
  | inpLeft i = v - 50 
  | inpRight i = v + 50 
  | otherwise = v
