module Application.Helper.View (
  module IHP.LoginSupport.Helper.View,
    generateCalenderHtml,splitYearMonthDay
                               ) where

import Prelude hiding (show) 
import IHP.ViewPrelude(Html,preEscapedToHtml,show)
import IHP.LoginSupport.Helper.View hiding (Day)
import Data.Char (isDigit)
import qualified Data.Text as T

-- Here you can add functions which are available in all your views
type YearMonthDay = T.Text 
type Year = Int
type Month = Int
type Day = Int
type Yobi = Int
type Duration = T.Text
type TTime = T.Text

yobiBase :: YearMonthDay 
yobiBase = "20200419" 

daylist :: [Day]
daylist = [31,28,31,30,31,30,31,31,30,31,30,31]

isUru :: Year -> Bool
isUru y = let r1 = mod y 4 == 0
              r2 = mod y 100 == 0
              r3 = mod y 400 == 0
           in r3 || (r1 && not r2)

howManyDays :: YearMonthDay -> YearMonthDay -> Day 
howManyDays sday fday = 
  let (y,m,d) = splitYearMonthDay sday 
      (yn,mn,dn) = splitYearMonthDay fday
      syear = min y yn
      fyear = max y yn
      fyday = 365 - daysFromTheYear sday + (if isUru y then 1 else 0)
      lsday = daysFromTheYear fday 
   in if y==yn then howManyDaysInYear y (m,d) (mn,dn) else
      if y+1==yn then fyday + lsday else fyday + daysBetweenYears (syear+1) (fyear-1) + lsday

howManyDaysInYear :: Year -> (Month,Day) -> (Month,Day) -> Day 
howManyDaysInYear y (m0,d0) (m1,d1) = 
  if m0 == m1 then abs (d1 - d0) else 
    let dm = abs (m1 - m0)
        stMonth = min m0 m1 
        fiMonth = max m0 m1
        uruEffect = if isUru y && stMonth <= 2 && stMonth+dm > 2 then 1 else 0
        fmday = if m1>m0 then daylist!!(m0-1) - d0 else daylist!!(m1-1) - d1 
        lmday = if m1>m0 then d1 else d0
     in if abs (m1-m0) == 1 then fmday + lmday + uruEffect
                            else daysBetweenMonths (stMonth + 1) (fiMonth - 1) + fmday + lmday + uruEffect 

daysBetweenYears :: Year -> Year -> Day 
daysBetweenYears syear fyear 
  | syear>fyear = daysBetweenYears fyear syear
  | syear==fyear = if isUru fyear then 366 else 365
  | otherwise = (if isUru syear then 366 else 365) + daysBetweenYears (syear+1) fyear 

daysBetweenMonths :: Month -> Month -> Day 
daysBetweenMonths smo fmo 
  | smo>fmo = daysBetweenYears fmo smo 
  | smo==fmo = daylist!!(smo-1) 
  | otherwise = daylist!!(smo-1) + daysBetweenMonths (smo+1) fmo 

daysFromTheYear :: YearMonthDay -> Day 
daysFromTheYear a = let (y,m,d) = splitYearMonthDay a
                     in if m==1 then d
                                else sum (take (m-1) daylist) + d + (if m>2 && isUru y then 1 else 0)

yobi :: YearMonthDay -> Yobi 
yobi day = let days = howManyDays yobiBase day 
               yobi = days `mod` 7
            in if day>=yobiBase then yobi else 7 - yobi

dayYobiList :: Year -> Month -> [(Day,Yobi)]
dayYobiList ye mo =
  let firstDay = show ye <> show mo <> "01"
      firstDaysYobi = yobi firstDay
      iu = isUru ye
      lastDay = if mo==2 && iu then 29 else daylist!!(mo-1)
   in zip [1..lastDay] ([firstDaysYobi..6]++cycle [0::Int,1..6])

weekDaysList :: [(Day,Yobi)] -> [[Day]]
weekDaysList [] = []
weekDaysList [(x,_)] = [[x]]
weekDaysList ((dy,wd):xs) 
  |wd==6 = [dy]:weekDaysList xs
  |otherwise = (dy:h) : t 
  where (h:t) = weekDaysList xs
  
filledWeekDaysList :: [[Day]] ->  [[Day]]
filledWeekDaysList dlst =
  let fweek = head dlst
      fwl = length fweek
      lweek = last dlst
      lwl = length lweek
      nfweek = replicate (7-fwl) 0 ++ fweek 
      nlweek = lweek ++ replicate (7-lwl) 0
   in [nfweek]++tail (init dlst)++[nlweek]

daysList :: Year -> Month -> [[Day]]
daysList ye mo = filledWeekDaysList (weekDaysList (dayYobiList ye mo))

isTTime :: T.Text -> Bool
isTTime txt = let tlen = T.length txt
               in tlen < 5 && T.all isDigit txt && 
             ((tlen < 3 && ((read (T.unpack txt))::Int) < 25) ||
              (tlen > 2 && ((read ((T.unpack) (T.takeEnd 2 txt)))::Int) < 60 
                        && ((read ((T.unpack) (T.dropEnd 2 txt)))::Int) < 25))

isDuration :: T.Text -> Bool
isDuration txt = let stxt = T.splitOn "-" txt
                  in length stxt == 2 && isTTime (head stxt) && isTTime (last stxt)

splitYearMonthDay :: YearMonthDay -> (Year, Month, Day)
splitYearMonthDay ymd =
  let (a:b:c:d:e:f:gs) = T.unpack ymd
      (yr,mo,da) = (read [a,b,c,d], read [e,f], read gs)
                         in if mo>12 then (yr,read [e],read (f:gs)) else (yr,mo,da)

generateCalenderHtml :: (T.Text -> T.Text) -> Year -> Month -> Html 
generateCalenderHtml link ye mo = preEscapedToHtml ("<table>"<>tbl<>"</table>\n")
  where dlst = daysList ye mo 
        hdr = foldl (\acc yb -> acc<>"<th>"<>yb<>"</th>") "" ["日","月","火","水","木","金","土"]
        mdl = foldl (\acc wk -> acc<>"<tr>"<>
                foldl (\acc dy -> acc<>"<td>"<>"<a"<>(link (show dy))<>">"
                    <>(if dy==0 then "" else show dy)<>"</a></td>") "" wk
                                           <>"</tr>") "" dlst
        tbl = hdr<>mdl 

