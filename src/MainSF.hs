{-# LANGUAGE Arrows #-}

module MainSF(mainSF) where

import FRP.Yampa (integral,returnA,sscan,SF,(^<<))
import System.Random (StdGen)

import Inputs (Inputs(..))
import MessageControl(messageControl)
import CharaControl(playerMove)
import EventControl(eventControl)
import WakaData (WakaData(..))

mainSF :: StdGen -> WakaData -> SF Inputs WakaData
mainSF rgen wd = proc i -> do
    rec
       let nwd = wd'{wdDouble=d,wdFieldData=fd,wdDialog=dl}
       dv <- (+(1::Double)) ^<< integral -< wdDouble wd 
       d  <- (+10)          ^<< integral -< dv
       wd'<- sscan eventControl wd       -< () 
       fd <- sscan playerMove fdWd       -< (i,inm) 
       dl <- sscan messageControl dlWd   -< (i,inm)
    returnA -< nwd 
    where fdWd = wdFieldData wd
          inm = wdInputMode wd
          dlWd = wdDialog wd
