{-# LANGUAGE OverloadedStrings #-}

module Day19 where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Text.Parsec
import Data.List
import Data.Ord

type Sub = (Text, Text)
type Molecule = Text

flipSubs :: [Sub] -> [Sub]
flipSubs = map (\(a, b) -> (b, a))

fewestSubs :: [Sub] -> Molecule -> Int
fewestSubs subs m
  | m == "e" = 0
  | otherwise = 1 + fewestSubs subs best
      where
        neighbors = performSubs subs m
        best = minimumBy (comparing T.length) neighbors

performSubs :: [Sub] -> Molecule -> [Molecule]
performSubs subs mol = subs >>= flip performSub mol

performSub :: Sub -> Molecule -> [Molecule]
performSub (a, b) mol = variations
  where
    bLength = T.length a
    pieces = T.breakOnAll a mol
    variations = handleBreak <$> pieces
    handleBreak (pre, post) = T.concat
      [ pre, b, T.drop bLength post ]

parsePuzzle :: String -> ([Sub], Molecule)
parsePuzzle s = p
  where
    (Right p) = parse puzzle "" s
    puzzle = (,) <$> subs <*> mol
    subs = sub `endBy` endOfLine
    sub = (,) <$> txt <*> (string " => " *> txt)
    mol = endOfLine *> txt
    txt = T.pack <$> many1 letter
