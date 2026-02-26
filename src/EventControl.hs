{-# LANGUAGE OverloadedStrings #-}

module EventControl(eventControl) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import WakaData (WakaData(..),Progress(..),SeneData(..),Senario)
import Names (StageName(..),SenarioName,SectionName)

eventControl :: WakaData -> () -> WakaData
eventControl wd _ = 
  let pr = wdProgress wd
   in case pr of
        (Progress False _ _ _) -> wd 
        (Progress True Opening srName scName) ->  
          let (SeneData pSrName _ pSenario) = wdSeneData wd
              nSenario = if srName==pSrName then pSenario
                                            else (wdGetSenario wd) srName
           in gameProgress nSenario Opening srName scName wd
        (Progress True _ _ _) -> wd

gameProgress :: Senario -> StageName -> SenarioName -> SectionName 
                                                    -> WakaData -> WakaData
gameProgress sr stgName srName scName wd = 
  let section = getTextSection scName sr  
      nsd = SeneData{senarioName=srName,sectionName=scName,senario=section} 
      npr = Progress False stgName srName scName
   in wd{wdSeneData=nsd,wdProgress=npr}

getTextSection :: SectionName -> Senario -> Text
getTextSection scName sr =
  let lbtxs = map (T.breakOn ":\n") (T.splitOn "\n\n" sr) 
   in T.drop 2 $ fromMaybe T.empty (lookup scName lbtxs)
