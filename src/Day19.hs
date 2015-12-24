module Day19 where

import Text.Parsec

type Sub = (String, String)
type Molecule = String

parsePuzzle :: String -> ([Sub], Molecule)
parsePuzzle s = p
  where
    (Right p) = parse puzzle "" s
    puzzle = (,) <$> subs <*> mol
    subs = sub `endBy` endOfLine
    sub = (,) <$> str <*> (string " => " *> str)
    mol = endOfLine *> str
    str = many1 letter
