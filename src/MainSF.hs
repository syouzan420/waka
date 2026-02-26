{-# LANGUAGE Arrows #-}

module MainSF(mainSF) where

import FRP.Yampa (integral,returnA,sscan,SF,(^<<))
import System.Random (StdGen)
import Data.Text (Text)

import Inputs (Inputs(..))
import MessageControl(messageControl)
import CharaControl(playerMove)
import WakaData (WakaData(..),Progress(..),SeneData(..),SectionName)
import Names (StageName(..))

mainSF :: StdGen -> WakaData -> SF Inputs WakaData
mainSF rgen wd = proc i -> do
    rec
       let nwd = wd'{wdDouble=d,wdFieldData=fd,wdDialog=dl}
       dv <- (+(1::Double)) ^<< integral -< wdDouble wd 
       d  <- (+10)          ^<< integral -< dv
       wd'<- sscan eventControl wd     -< prWd 
       fd <- sscan playerMove fdWd       -< (i,inm) 
       dl <- sscan messageControl dlWd   -< (i,inm)
    returnA -< nwd 
    where fdWd = wdFieldData wd
          inm = wdInputMode wd
          dlWd = wdDialog wd
          prWd = wdProgress wd

eventControl :: WakaData -> Progress -> WakaData
eventControl wd (Progress False _ _ _) = wd 
eventControl wd (Progress True Opening senarioName sectionName) =  
  let (SeneData pSenarioName _ pSenario) = wdSeneData wd
      nSenario = if senarioName==pSenarioName then pSenario
                                              else (wdGetSenario wd) senarioName
   in gameProgress nSenario sectionName wd
eventControl wd (Progress True _ _ _) = wd

gameProgress :: Text -> SectionName -> WakaData -> WakaData
gameProgress _ _ wd = wd
