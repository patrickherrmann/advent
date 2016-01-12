module Day10 where

import Data.List

repeatLookAndSay :: Int -> String -> String
repeatLookAndSay n = (!! n) . iterate lookAndSay

lookAndSay :: String -> String
lookAndSay = (>>= say) . group
  where say g = show (length g) ++ take 1 g