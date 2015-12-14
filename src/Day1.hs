module Day1
  ( floor
  , position
  ) where

import Prelude hiding (floor)

type Floor = Int
type Position = Int

floor :: String -> Floor
floor [] = 0
floor ('(':cs) = floor cs + 1
floor (')':cs) = floor cs - 1

position :: String -> Position
position s = position' s 0 0

position' :: String -> Floor -> Position -> Position
position' _ (-1) p = p
position' ('(':cs) f p = position' cs (f + 1) (p + 1)
position' (')':cs) f p = position' cs (f - 1) (p + 1)