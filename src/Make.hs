module Make where

import Data.Point2 (Point2(..))
import Data.Vector2 (vector2)
import Data.AffineSpace ((.+^))
import WakaData (DialogBox(..),Mozi(..),Rect(..),TextDir(..),Position)

data TxMode = Normal | Rubi | Func deriving Eq 
type TempPos = Position -- temporal position for rubi (Previous normal position)

makeMoziData :: DialogBox -> String -> [Mozi]
makeMoziData = makeMoziData' Normal (Point2 0 0) (Point2 0 0) 

makeMoziData' :: TxMode -> TempPos -> Position
                               -> DialogBox -> String -> [Mozi]
makeMoziData' _ _ _ _ [] = []
makeMoziData' txMode tmpPos pos db (ch:xs) =
  case txMode of 
        Normal -> if nTxMode/=Normal then next tmpPos pos db
                                     else let (nPos,mz) = makeMozi ch Normal pos db
                                           in mz:next tmpPos nPos db
        _other -> next tmpPos pos db 
  where 
    nTxMode = case ch of
        '：' -> case txMode of 
                  Normal -> Rubi; Rubi -> Normal; _other -> txMode
        '_'  -> case txMode of
                  Normal -> Func; Func -> Normal; _other -> txMode
        _other -> txMode 
    next ntp nps ndb = makeMoziData' nTxMode ntp nps ndb xs

makeMozi :: Char -> TxMode -> Position -> DialogBox -> (Position,Mozi)
makeMozi ch txMode p@(Point2 px py) db =
  (npos,Mozi ft nfs (p.+^vector2 x y) ch)
  where 
    td = textDir db
    (Rect x y w h) = dialogRect db
    ft = fontType db
    fs = fontSize db
    rfs = fs/3
    tf = textFeed db
    lf = lineFeed db
    nfs = if txMode==Normal then fs else rfs
    npos
      | txMode == Normal = if td==Tate then 
          let tpy = py+tf
              npy = if tpy>h then 0 else tpy
              npx = if tpy>h then px-lf else px
           in Point2 npx npy
                                       else
          let tpx = px+tf
              npx = if tpx>w then 0 else tpx
              npy = if tpx>w then py+lf else py
           in Point2 npx npy
      | otherwise = p
