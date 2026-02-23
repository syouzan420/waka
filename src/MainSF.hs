{-# LANGUAGE Arrows #-}

module MainSF(mainSF) where

import FRP.Yampa (integral,returnA,sscan,SF,(^<<))
import System.Random (StdGen)
import Foreign.C.Types (CFloat)
import Data.Vector2 (vector2)
import Data.AffineSpace ((.+^))
import Data.Point2 (Point2(..))

import Inputs (Inputs(..))
import WakaData (WakaData(..),ImgType(..),ImgLR(..),ImgDir(..),FieldData(..)
                ,InputMode(..),Dialog(..))

mainSF :: StdGen -> WakaData -> SF Inputs WakaData
mainSF rgen wd = proc i -> do
    rec
       let nwd = wd{wdDouble=d,wdFieldData=fd,wdDialog=dl}
       dv <- (+(1::Double)) ^<< integral -< wdDouble wd 
       d  <- (+10)          ^<< integral -< dv
       fd <- sscan playerMove fdWd       -< (i,inm) 
       dl <- sscan messageControl dlWd   -< (i,inm)
    returnA -< nwd 
    where fdWd = wdFieldData wd
          inm = wdInputMode wd
          dlWd = wdDialog wd

messageControl :: [Dialog] -> (Inputs,InputMode) -> [Dialog]
messageControl [] _ = []
messageControl dl@(d:ds) (i,IDialog) 
  | inpA i = if isstop then d{isStop=False}:ds else dl
  | otherwise = messagesUpdate dl
  where isstop = isStop d
messageControl dl _ = messagesUpdate dl

messagesUpdate :: [Dialog] -> [Dialog]
messagesUpdate = map messageUpdate  

messageUpdate :: Dialog -> Dialog
messageUpdate d = let isstop = isStop d; isend = isEnd d
                      tp = textPosition d
                      tc = textCount d; tcmax = textCountMax d
                   in if isstop || isend then d else 
                          let ntp = if tc==tcmax then tp+1 else tp
                              ntc = if tc==tcmax then 0 else tc+1
                           in d{textPosition=ntp,textCount=ntc}

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
