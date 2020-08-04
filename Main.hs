module Main where

import Date
import Estimation

main :: IO ()
main = prettyEstimate [(start, 35752 ::Double), (middle, 36030), (end, 36094)] 10.12 0.2242
    where start = DayMonth 30 3
          middle = DayMonth 13 7
          end = DayMonth 2 8
