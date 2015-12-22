module Day17 where

import Control.Monad

numberOfCombos :: Int -> [Int] -> Int
numberOfCombos n cs = length $ checkCapacities n cs

numberOfMinimumCombos :: Int -> [Int] -> Int
numberOfMinimumCombos n cs = length $ filter ((==minLength) . length) combos
  where
    combos = checkCapacities n cs
    minLength = minimum $ map length combos

checkCapacities :: Int -> [Int] -> [[Int]]
checkCapacities n = filter (fitsExactly n) . powerset

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

fitsExactly :: Int -> [Int] -> Bool
fitsExactly n is = sum is == n

parseCapacities :: String -> [Int]
parseCapacities = map read . lines