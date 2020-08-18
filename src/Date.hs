module Date where

type Day = Int

type Month = Int

type Year = Int

data Date
  = DayMonth Day Month
  | DayMonthYear Day Month Year
  deriving (Show, Ord, Eq)

daysOfMonth :: (Integral a) => a -> a
daysOfMonth x
  | x <= 0 || x > 12 = error "Month must be between 0 and 13"
  | x == 2 = 28
  | x == 9 || x == 11 = 30
  | x == 8 || x == 10 || x == 12 = 31
  | odd x = 31
  | even x = 30

durationMonths :: [Month] -> Int
durationMonths []       = 0
durationMonths (x : xs) = daysOfMonth x + durationMonths xs

exclusiveList :: (Integral a) => a -> a -> [a]
exclusiveList s e
  | e < s = error "Start must be smaller than end"
  | otherwise = [s + 1 .. e - 1]

duration :: Date -> Date -> Int
duration s@(DayMonth sd sm) e@(DayMonth ed em) = daysOfMonth sm - sd + ed + durationMonths (exclusiveList sm em)
duration s@(DayMonthYear sd sm sy) e@(DayMonthYear ed em ey)
  | ey == sy = duration s e
  | otherwise = duration s endOfYear + duration startOfYear e + sum (exclusiveList sy ey) * 365
  where
    endOfYear = DayMonth 31 12
    startOfYear = DayMonth 1 1
