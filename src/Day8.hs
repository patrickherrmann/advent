{-# LANGUAGE ViewPatterns #-}

module Day8 where

totalCharsDiff :: String -> Int
totalCharsDiff s = sum $ map charDiff $ lines s

charDiff :: String -> Int
charDiff s = length s + 2 - chars s

chars :: String -> Int
chars ('\\':'\\':cs) = 1 + chars cs
chars ('\\':'"':cs) = 1 + chars cs
chars ('\\':'x':(isHex -> True):(isHex -> True):cs) = 1 + chars cs
chars (_:cs) = 1 + chars cs
chars [] = 0

isHex :: Char -> Bool
isHex = (`elem` hexChars)
  where hexChars = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']