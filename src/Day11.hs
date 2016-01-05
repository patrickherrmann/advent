module Day11 where

import Data.List

nextPassword :: String -> String
nextPassword s = reverse next
  where
    (_:ps) = iterate incPassword (reverse s)
    (Just next) = find isValidPassword ps

-- The following functions operate on *reversed* passwords,
-- so that incrementing them is a more efficient cons operation

incPassword :: String -> String
incPassword [] = "a"
incPassword ('z':cs) = 'a' : incPassword cs
incPassword (c:cs) = next c : cs
  where
    next 'h' = 'j'
    next 'n' = 'p'
    next 'k' = 'm'
    next x = succ x

isValidPassword :: String -> Bool
isValidPassword s = not (containsIllegalChars s)
  && containsDoubles s
  && containsRun s

containsIllegalChars :: String -> Bool
containsIllegalChars s = any (`elem` s) illegalChars
  where illegalChars = ['i', 'o', 'l']

containsDoubles :: String -> Bool
containsDoubles s = length pairs >= 2
  where pairs = filter (\g -> length g == 2) (group s)

containsRun :: String -> Bool
containsRun (c:r@(b:a:_)) = (c == succ b && b == succ a)
  || containsRun r
containsRun _ = False