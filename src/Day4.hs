module Day4 where

import Data.List
import Data.Hash.MD5

findAdventCoin :: String -> Int -> Int
findAdventCoin key zs = i
  where (Just i) = find (makesAdventCoin key zs) [1..]

makesAdventCoin :: String -> Int -> Int -> Bool
makesAdventCoin key zs i = zeros `isPrefixOf` hash
  where
    hash = md5s (Str $ key ++ show i)
    zeros = replicate zs '0'