module MessageControl(messageControl) where

import Inputs (Inputs(..))
import WakaData (InputMode(..),Dialog(..))

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
