module Day10 where

import Data.List

lookAndSayX40 :: String -> String
lookAndSayX40 = (!! 40) . iterate lookAndSay

lookAndSay :: String -> String
lookAndSay s = concatMap say $ group s
  where say g = show (length g) ++ take 1 g