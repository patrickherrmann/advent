module Day1 where

finalFloor :: String -> Int
finalFloor [] = 0
finalFloor ('(':cs) = finalFloor cs + 1
finalFloor (')':cs) = finalFloor cs - 1

position :: String -> Int
position s = go s 0 0
  where
    go _ (-1) p = p
    go ('(':cs) f p = go cs (f + 1) (p + 1)
    go (')':cs) f p = go cs (f - 1) (p + 1)