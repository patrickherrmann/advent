module Day01 where

finalFloor :: String -> Int
finalFloor = \case
  [] -> 0
  '(':cs -> finalFloor cs + 1
  ')':cs -> finalFloor cs - 1

position :: String -> Int
position s = go s 0 0
  where
    go :: String -> Int -> Int -> Int
    go _ (-1) p = p
    go ('(':cs) f p = go cs (f + 1) (p + 1)
    go (')':cs) f p = go cs (f - 1) (p + 1)