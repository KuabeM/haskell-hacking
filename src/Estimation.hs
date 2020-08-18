module Estimation where

import           Data.List
import           Date
import           Text.Printf

usage :: (Num a, Ord a, Fractional a) => a -> a -> a
usage a b
  | a <= b = b - a
  | a > b = error "Measurements must me continuously increasing"

usagePerPeriod :: (Num a, Ord a, Fractional a) => [(Date, a)] -> [(Int, a)]
usagePerPeriod [] = []
usagePerPeriod [x] = []
usagePerPeriod ((s, x) : (e, y) : xs) = [(duration s e, usage x y)] ++ usagePerPeriod ([(e, y)] ++ xs)

averageUsageInPeriod :: (Num a, Fractional a) => [(Int, a)] -> [a]
averageUsageInPeriod [] = []
averageUsageInPeriod ((d, x) : xs) = [x / fromIntegral d] ++ averageUsageInPeriod xs

calcAverages :: (Num a, Ord a, Fractional a) => [(Date, a)] -> [a]
calcAverages []     = []
calcAverages period = averageUsageInPeriod (usagePerPeriod period)

average :: (Fractional a) => [a] -> a
average x = sum x / fromIntegral (length x)

estimateYear :: (Fractional a) => [a] -> a
estimateYear avgPerPeriod = average avgPerPeriod * 365

estimateCosts :: (Fractional a) => a -> a -> [a] -> a
estimateCosts baseCost costKwh avgPerPeriod = baseCost * 12 + estimateYear avgPerPeriod * costKwh

calcEstimatedCosts :: (Fractional a, Ord a) => [(Date, a)] -> a -> a -> (a, a)
calcEstimatedCosts consumption baseCosts costKwh = (estimateCosts baseCosts costKwh avg, estimateYear avg)
  where
    avg = calcAverages consumption

prettyEstimate :: (Fractional a, Ord a, Show a, PrintfArg a) => [(Date, a)] -> a -> a -> IO ()
prettyEstimate consumption baseCosts costKwh = putStrLn ("A consumption of " ++ printf "%.2f" p ++ "kWh will cost you " ++ printf "%.2f" c ++ "€.")
  where
    (c, p) = calcEstimatedCosts consumption baseCosts costKwh
