module Day2 where

import Control.Applicative
import Data.List.Split

type Box = (Int, Int, Int)

totalPaperRequired :: String -> Int
totalPaperRequired s = sum subtotals
  where subtotals = paperRequired . readBox <$> lines s

paperRequired :: Box -> Int
paperRequired (l, w, h) = 2 * sum sides + minimum sides
  where sides = [l * w, l * h, w * h]

readBox :: String -> Box
readBox s = (l, w, h)
  where [l, w, h] = read <$> splitOn "x" s