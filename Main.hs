module Main where

import           Date
import           Estimation

main :: IO ()
main = putStrLn $ estimation ++ " " ++ avgs
  where
    start = DayMonth 30 3
    middle = DayMonth 13 7
    end = DayMonth 2 8
    next = DayMonth 31 8
    estimation = prettyEstimate [(start, 35752 :: Double), (middle, 36030), (end, 36094), (next, 36136)] 10.12 0.2242
    avgs = prettyAveragesPerPeriod [(start, 35752 :: Double), (middle, 36030), (end, 36094), (next, 36136)]
