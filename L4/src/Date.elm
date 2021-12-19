
module Date exposing (Month(..), createDate, daysInMonth, monthToInt, compareMonth, isLeapYear)

type Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
type Date = Date {day: Int, month: Month, year: Int}

createDate : Int -> Month -> Int -> Maybe Date
createDate day month year =
  let
    between start end num = (start <= num) && (num <= end)
    lastDay = daysInMonth year month
  in
    if not (between 1 lastDay day) then
      Nothing
    else if not (between 1970 3000 year) then
      Nothing
    else
      Just (Date {day = day, month = month, year = year})

isLeapYear : Int -> Bool
isLeapYear year = 
        if ((modBy 4 year == 0 ) && (modBy 100 year /= 0)) || (modBy 400 year == 0) then True
        else False

daysInMonth : Int -> Month -> Int
daysInMonth year month =
  case month of
    Jan -> 31
    Feb -> if isLeapYear year then 28 else 29
    Mar -> 31
    Apr -> 30
    May -> 31
    Jun -> 30
    Jul -> 31
    Aug -> 31
    Sep -> 30
    Oct -> 31
    Nov -> 30
    Dec -> 31

monthToInt : Month -> Int
monthToInt month =
  case month of
    Jan -> 1
    Feb -> 2
    Mar -> 3
    Apr -> 4
    May -> 5
    Jun -> 6
    Jul -> 7
    Aug -> 8
    Sep -> 9
    Oct -> 10
    Nov -> 11
    Dec -> 12

compareMonth : Month -> Month -> Order
compareMonth m1 m2 =
  compare (monthToInt m1) (monthToInt m2)

