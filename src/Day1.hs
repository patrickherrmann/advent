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
position s = go s 0 0
  where
    go _ (-1) p = p
    go ('(':cs) f p = go cs (f + 1) (p + 1)
    go (')':cs) f p = go cs (f - 1) (p + 1)