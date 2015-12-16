module Day3 where

import Data.List

type Point = (Int, Int)

houses :: String -> Int
houses s = go s [(0, 0)]
  where
    go [] vs = length $ nub vs
    go (c:cs) vs@(v:_) = go cs (move v c:vs)

move :: Point -> Char -> Point
move (x, y) d = case d of
  '>' -> (x + 1, y)
  '<' -> (x - 1, y)
  '^' -> (x, y + 1)
  'v' -> (x, y - 1)