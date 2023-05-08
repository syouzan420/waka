module Application.Helper.Controller (
  module IHP.LoginSupport.Helper.Controller,
  isDuration
                                     ) where

import Prelude hiding (show) 
import IHP.ControllerPrelude hiding (last,head)
import IHP.LoginSupport.Helper.Controller
import Generated.Types
import Data.Char (isDigit)
import qualified Data.Text as T


type instance CurrentUserRecord = User

-- Here you can add functions which are available in all your controllers
isTTime :: T.Text -> Bool
isTTime txt = let tlen = T.length txt
               in tlen < 5 && T.all isDigit txt && 
             ((tlen < 3 && ((read (T.unpack txt))::Int) < 25) ||
              (tlen > 2 && ((read ((T.unpack) (T.takeEnd 2 txt)))::Int) < 60 
                        && ((read ((T.unpack) (T.dropEnd 2 txt)))::Int) < 25))

isDuration :: T.Text -> ValidatorResult 
isDuration txt = let stxt = T.splitOn "-" txt
                     b = length stxt == 2 && isTTime (head stxt) && isTTime (last stxt)
                  in if b then Success
                          else Failure "9時から10時なら 9-10 みたいに入れてね"

