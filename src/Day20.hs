module Day20 where

import Data.List

guessSmallestFactor :: Int -> Int
guessSmallestFactor n = i
  where (Just i) = find ((>=n).presents) [320000, 320320..]

guessSmallestFactor2 :: Int -> Int
guessSmallestFactor2 n = i
  where (Just i) = find ((>=n).presents2) [320000, 320320..]

factors :: Int -> [Int]
factors n = 1 : n : [x | x <- [2..n2], n `rem` x == 0]
  where n2 = n `div` 2 + n `rem` 2

factors2 :: Int -> [Int]
factors2 n = n : [x | x <- [1..n2], n `rem` x == 0, x * 50 >= n]
  where n2 = n `div` 2 + n `rem` 2

presents2 :: Int -> Int
presents2 = sum . map (*11) . factors2

presents :: Int -> Int
presents = sum . map (*10) . factors

parseInput :: String -> Int
parseInput = read