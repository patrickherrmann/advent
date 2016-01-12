module Day05 where

import Data.List

niceCount :: (String -> Bool) -> String -> Int
niceCount isNice = length . filter isNice . lines

isNice1 :: String -> Bool
isNice1 s = hasEnoughVowels s
            && not (hasIllegalPair s)
            && hasDoubleLetter s

isNice2 :: String -> Bool
isNice2 s = hasRecurringPair s && hasSandwich s

hasEnoughVowels :: String -> Bool
hasEnoughVowels = (== 3) . length . take 3 . filter isVowel
  where isVowel c = c `elem` "aeiou"

hasIllegalPair :: String -> Bool
hasIllegalPair s = any (`isInfixOf` s) bannedGroups
  where bannedGroups = ["ab", "cd", "pq", "xy"]

hasDoubleLetter :: String -> Bool
hasDoubleLetter = any ((>1) . length) . group

hasRecurringPair :: String -> Bool
hasRecurringPair = \case
  a:b:cs -> [a, b] `isInfixOf` cs || hasRecurringPair (b:cs)
  _      -> False

hasSandwich :: String -> Bool
hasSandwich = \case
  a:b:c:cs -> a == c || hasSandwich (b:c:cs)
  _        -> False