module Day5 where

import Data.List
import Data.List.Split

niceCount :: String -> Int
niceCount = length . filter isNice . lines

isNice :: String -> Bool
isNice s = hasEnoughVowels s
            && not (hasIllegalPair s)
            && hasDoubleLetter s

hasEnoughVowels :: String -> Bool
hasEnoughVowels = (== 3) . length . take 3 . filter isVowel
  where isVowel c = c `elem` "aeiou"

hasIllegalPair :: String -> Bool
hasIllegalPair s = any (`isInfixOf` s) bannedGroups
  where bannedGroups = ["ab", "cd", "pq", "xy"]

hasDoubleLetter :: String -> Bool
hasDoubleLetter = any ((>1) . length) . group