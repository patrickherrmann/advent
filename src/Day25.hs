module Day25 where

import Data.Char
import Data.Maybe
import Safe

codeSequence :: [Int]
codeSequence = iterate next 20151125
  where next c = (c * 252533) `rem` 33554393

getIndex :: (Int, Int) -> Int
getIndex (row, col) = column !! (row - 1)
  where
    firstRow = 1 : zipWith (+) firstRow [2..]
    column = (firstRow !! (col - 1)) : zipWith (+) column [col..]

parseCoordinates :: String -> (Int, Int)
parseCoordinates s = (row, col)
  where
    [row, col] = catMaybes $ readMay <$> words stripped
    stripped = filter (not . isPunctuation) s