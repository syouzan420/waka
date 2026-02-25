module Make where

import Data.Point2 (Point2(..))
import WakaData (DialogBox(..),Mozi(..),Rect(..),Position)

type IsRubi = Bool
type IsFunc = Bool
type TempPos = Position -- temporal position for rubi (Previous normal position)

makeMoziData :: DialogBox -> String -> [Mozi]
makeMoziData = makeMoziData' False False (Point2 0 0) (Point2 0 0) 

makeMoziData' :: IsRubi -> IsFunc -> TempPos -> Position
                               -> DialogBox -> String -> [Mozi]
makeMoziData' _ _ _ _ _ [] = []
makeMoziData' isRubi isFunc tmpPos (Point2 px py) db (ch:xs) =
  let nIsRubi = if ch=='：' then not isRubi else isRubi 
      nIsFunc = if ch=='_' then not isFunc else isFunc
   in undefined
  where td = textDir db
        (Rect x y w h) = dialogRect db
        ft = fontType db
        fs = fontSize db
        rfs = fs/3
        tf = textFeed db
        lf = lineFeed db

