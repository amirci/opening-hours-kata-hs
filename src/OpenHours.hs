module OpenHours where

import Data.List
import Data.Dates
import Data.List.Split
import Data.Maybe

data Hours = Hours {days :: [WeekDay], openTime :: Time, closeTime :: Time} 

schedule dow open close = 
  let
    parse t = map (read :: String -> Int) $ splitOn ":" t
    toTime [a, b] = Time a b 0
  in Hours dow (toTime $ parse open) $ toTime $ parse close

isOpenOn date hrs = 
  let
    sameDay = elem (dateWeekDay date) $ days hrs 
    time = Time (hour date) (minute date) 0
    betweenHours = time >= openTime hrs && time <= closeTime hrs
  in sameDay && betweenHours

nextOpeningDate dateTime hrs =
  let
    zero = dayToDateTime $ dateTimeToDay dateTime 
    startDate = addTime zero $ openTime hrs
    nextOpen  = flip isOpenOn hrs
  in fromJust $ find nextOpen $ map (addInterval startDate . Days) [1..]
 
