module Estimation where

import Date
import Data.List
import Text.Printf

usage :: (Num a, Ord a, Fractional a) => a -> a -> a
usage a b
  | a <= b = b - a
  | a > b = a - b

usagePerPeriod :: (Num a, Ord a, Fractional a) => [(Date, a)] -> [(Int, a)]
usagePerPeriod [] = []
usagePerPeriod [x] = []
usagePerPeriod ((s, x) : (e, y) : xs) = [(duration s e, usage x y)] ++ usagePerPeriod ([(e, y)] ++ xs)

averageUsageInPeriod :: (Num a, Fractional a) => [(Int, a)] -> [a]
averageUsageInPeriod [] = []
averageUsageInPeriod ((d, x) : xs) = [x / fromIntegral d] ++ averageUsageInPeriod xs

calcAverages :: (Num a, Ord a, Fractional a) => [(Date, a)] -> [a]
calcAverages [] = []
calcAverages x = averageUsageInPeriod (usagePerPeriod x)

average :: (Fractional a) => [a] -> a
average x = sum x / fromIntegral (length x)

estimateYear :: (Fractional a) => [a] -> a
estimateYear x = average x * 365

estimateCosts :: (Fractional a) => a -> a -> [a] -> a
estimateCosts g k avg = g * 12 + estimateYear avg * k

calcEstimatedCosts :: (Fractional a, Ord a) => [(Date, a)] -> a -> a -> (a, a)
calcEstimatedCosts x g k = (estimateCosts g k avg, estimateYear avg)
  where avg = calcAverages x

prettyEstimate :: (Fractional a, Ord a, Show a, PrintfArg a) => [(Date, a)] -> a -> a -> IO ()
prettyEstimate x g k = putStrLn ("A consumption of " ++ printf "%.2f" p ++ "kWh will cost you " ++ printf "%.2f" c ++ "€.")
  where (c, p) = calcEstimatedCosts x g k