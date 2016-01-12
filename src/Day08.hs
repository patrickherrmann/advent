module Day08 where

totalCharsDiff :: String -> Int
totalCharsDiff = sum . map charDiff . lines

totalCharsDiff2 :: String -> Int
totalCharsDiff2 = sum . map charDiff2 . lines

charDiff2 :: String -> Int
charDiff2 s = specialChars s + 2

charDiff :: String -> Int
charDiff s = length s + 2 - chars s

chars :: String -> Int
chars = \case
  '\\':'\\':cs -> 1 + chars cs
  '\\':'"':cs -> 1 + chars cs
  '\\':'x':(isHex -> True):(isHex -> True):cs -> 1 + chars cs
  _:cs -> 1 + chars cs
  [] -> 0

specialChars :: String -> Int
specialChars = length . filter isSpecial
  where isSpecial c = c == '\\' || c == '"'

isHex :: Char -> Bool
isHex = (`elem` hexChars)
  where hexChars = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']