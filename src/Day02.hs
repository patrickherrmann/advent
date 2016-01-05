module Day02 where

import Data.List.Split

type Box = (Int, Int, Int)

totalPaperRequired :: String -> Int
totalPaperRequired = sum . map paperRequired . readBoxes

paperRequired :: Box -> Int
paperRequired (l, w, h) = 2 * sum sides + minimum sides
  where sides = [l * w, l * h, w * h]

totalRibbonRequired :: String -> Int
totalRibbonRequired = sum . map ribbonRequired . readBoxes

ribbonRequired :: Box -> Int
ribbonRequired (l, w, h) = wrap + bow
  where
    wrap = 2 * minimum [l + w, l + h, w + h]
    bow = l * w * h

readBoxes :: String -> [Box]
readBoxes s = readBox <$> lines s

readBox :: String -> Box
readBox s = (l, w, h)
  where [l, w, h] = read <$> splitOn "x" s