module Day1
  ( finalFloor
  , position
  ) where

type Floor = Int
type Position = Int

finalFloor :: String -> Floor
finalFloor [] = 0
finalFloor ('(':cs) = finalFloor cs + 1
finalFloor (')':cs) = finalFloor cs - 1

position :: String -> Position
position s = go s 0 0
  where
    go _ (-1) p = p
    go ('(':cs) f p = go cs (f + 1) (p + 1)
    go (')':cs) f p = go cs (f - 1) (p + 1)