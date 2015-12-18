{-# LANGUAGE ViewPatterns #-}

module Day8 where

totalCharsDiff :: String -> Int
totalCharsDiff = sum . map charDiff . lines

totalCharsDiff2 :: String -> Int
totalCharsDiff2 = sum . map charDiff2 . lines

charDiff2 :: String -> Int
charDiff2 s = specialChars s + 2

charDiff :: String -> Int
charDiff s = length s + 2 - chars s

chars :: String -> Int
chars ('\\':'\\':cs) = 1 + chars cs
chars ('\\':'"':cs) = 1 + chars cs
chars ('\\':'x':(isHex -> True):(isHex -> True):cs) = 1 + chars cs
chars (_:cs) = 1 + chars cs
chars [] = 0

specialChars :: String -> Int
specialChars = length . filter isSpecial
  where isSpecial c = c == '\\' || c == '"'

isHex :: Char -> Bool
isHex = (`elem` hexChars)
  where hexChars = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']