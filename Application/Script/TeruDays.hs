{-# LANGUAGE OverloadedStrings #-}
module Application.Script.TeruDays (YearMonthDay,Year,Month,Day,weeklist,howLong,
  howManyDays,daysFromTheYear,daysBetweenYears,daysList,splitYearMonthDay) where

import Prelude
import Data.Semigroup (Semigroup,(<>))
import Data.String (IsString)
import Data.Time.LocalTime(getZonedTime,ZonedTime(zonedTimeToLocalTime),LocalTime(localDay))
import qualified Data.Text as T

type YearMonthDay = T.Text 
type HourMinute = T.Text 
type Year = Int; type Month = Int; type Day = Int; type Hour = Int; type Minute = Int

daylist :: [Int]
daylist = [31,28,31,30,31,30,31,31,30,31,30,31]

weeklist = ["su","m","tu","w","th","f","sa"]

yobiBase :: YearMonthDay 
yobiBase = "20200419" 

howLong :: HourMinute -> HourMinute -> Minute 
howLong s f =
  let (sho,smi) = splitHourMinute s
      (fho,fmi) = splitHourMinute f
      sami = sho * 60 + smi
      fami = fho * 60 + fmi
   in fami - sami

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

isUru :: Year -> Bool
isUru y = let r1 = mod y 4 == 0
              r2 = mod y 100 == 0
              r3 = mod y 400 == 0
           in r3 || (r1 && not r2)

splitHourMinute :: HourMinute -> (Hour, Minute)
splitHourMinute s =
  let len = T.length s
      (ho,mi) = if len==3 then (T.pack [T.head s],T.tail s) else T.splitAt 2 s 
   in (read (T.unpack ho), read (T.unpack mi))

splitYearMonthDay :: YearMonthDay -> (Year, Month, Day)
splitYearMonthDay ymd =
  let (a:b:c:d:e:f:gs) = T.unpack ymd
      (yr,mo,da) = (read [a,b,c,d], read [e,f], read gs)
                         in if mo>12 then (yr,read [e],read (f:gs)) else (yr,mo,da)

daysFromTheYear :: YearMonthDay -> Day 
daysFromTheYear a = let (y,m,d) = splitYearMonthDay a
                     in if m==1 then d
                                else sum (take (m-1) daylist) + d + (if m>2 && isUru y then 1 else 0)


yobi :: YearMonthDay -> Int
yobi day = let days = howManyDays yobiBase day 
               yobi = days `mod` 7
            in if day>=yobiBase then yobi else 7 - yobi

dayYobiList :: Year -> Month -> [(Day,Int)]
dayYobiList ye mo =
  let firstDay = T.pack (show ye) <> T.pack (show mo) <> "01"
      firstDaysYobi = yobi firstDay
      iu = isUru ye
      lastDay = if mo==2 && iu then 29 else daylist!!(mo-1)
   in zip [1..lastDay] ([firstDaysYobi..6]++cycle [0::Int,1..6])

weekDaysList :: [(Day,Int)] -> [[Day]]
weekDaysList [] = []
weekDaysList [(x,_)] = [[x]]
weekDaysList ((dy,wd):xs) 
  |wd==6 = [dy]:weekDaysList xs
  |otherwise = (dy:h) : t 
  where (h:t) = weekDaysList xs
  
filledWeekDaysList :: [[Day]] -> [[Day]]
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

