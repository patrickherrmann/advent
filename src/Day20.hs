module Day20 where

import Data.List

guessSmallestFactor :: Int -> Int
guessSmallestFactor n = i
  where (Just i) = find ((>=n).presents) [320000, 320320..]

factors :: Int -> [Int]
factors n =1 : n : [x | x <- [2..n2], n `rem` x == 0]
  where n2 = n `div` 2 + n `rem` 2

presents :: Int -> Int
presents = sum . map (*10) . factors

parseInput :: String -> Int
parseInput = read