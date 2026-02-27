{-# LANGUAGE OverloadedStrings #-}

module EventControl(eventControl) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Point2 (Point2(..))

import MakeMoziData (makeMoziData)
import WakaData (WakaData(..),Progress(..),SeneData(..),Dialog(..),DialogBox(..)
                ,Mozi(..),TextDir(..),Sene,Senario,defaultDialog,defaultDialogBox)
import Names (StageName(..),SectionName)

eventControl :: WakaData -> () -> WakaData
eventControl wd _ = 
  let pr = wdProgress wd
   in case pr of
        (Progress False _ _) -> wd 
        (Progress True stgName senes) ->  
          let sn = last senes
              (SeneData pSene pSenario) = wdSeneData wd
              isSameSection = sn==pSene
              nSenario = if isSameSection then pSenario
                                          else (wdGetSenario wd) (fst sn) 
           in gameProgress isSameSection nSenario stgName senes wd

gameProgress :: Bool -> Senario -> StageName -> [Sene] -> WakaData -> WakaData
gameProgress isSame sr stgName senes wd = 
  let dlgs = wdDialog wd
      iDlgs = if null dlgs then [] else init dlgs
      dboxes = wdDialogBox wd
      iDboxes = if null dboxes then [] else init dboxes
      sn@(srName,scName) = last senes 
      section = if isSame then sr else getTextSection scName sr  
      sList = T.lines section
      (fn,rm) = span (\l -> T.head l=='/') sList
      nwd = if null fn then wd else execSenario fn wd
      (ms,rm2) = span (\l -> T.head l/='/') rm
      mstr = (T.unpack . T.unlines) ms
      dbox = if null dboxes then defaultDialogBox else last dboxes
      pdlg = if null dlgs then defaultDialog else last dlgs 
      dlg = addToDialog dbox mstr pdlg
      nsd = SeneData{sene=sn,senario=T.unlines rm2} 
      npr = Progress False stgName senes 
   in nwd{wdSeneData=nsd,wdProgress=npr
         ,wdDialog=iDlgs++[dlg],wdDialogBox=iDboxes++[dbox]}

addToDialog :: DialogBox -> String -> Dialog -> Dialog
addToDialog dbox mstr dlg =
  let pdata = textData dlg
      pos = if null pdata then (Point2 0 0) else
        let (Mozi _ _ p _) = last pdata in p
      mdata = makeMoziData dbox pos mstr 
   in dlg{textData=pdata++mdata}

execSenario :: [Text] -> WakaData -> WakaData
execSenario [] wd = wd 
execSenario (x:xs) wd =
  let (f:args) = (T.words . T.tail) x 
      nwd = case f of 
              "setDialog" -> setDialog args wd 
              "newLine" -> newLine wd 
              "getMessage" -> getMessage args wd 
              _ -> wd
   in execSenario xs nwd

getTextSection :: SectionName -> Senario -> Text
getTextSection scName sr =
  let lbtxs = map (T.breakOn ":\n") (T.splitOn "\n\n" sr) 
   in T.drop 2 $ fromMaybe T.empty (lookup scName lbtxs)

setDialog :: [Text] -> WakaData -> WakaData      
setDialog [] wd = wd 
setDialog (x:xs) wd =
  let dboxes = wdDialogBox wd
      dbox = defaultDialogBox 
      nwd = case x of
            "tate" -> wd{wdDialogBox=dboxes++[dbox{textDir=Tate}]}  
            "yoko" -> wd{wdDialogBox=dboxes++[dbox{textDir=Yoko}]}  
            "noBorder" -> wd{wdDialogBox=dboxes++[dbox{isBorder=False}]}  
            "border" -> wd{wdDialogBox=dboxes++[dbox{isBorder=True}]}  
            _ -> wd
   in setDialog xs nwd


newLine :: WakaData -> WakaData
newLine wd = wd 

getMessage :: [Text] -> WakaData -> WakaData
getMessage [] wd = wd
getMessage [srName,scName] wd = wd 
getMessage _ wd = wd

