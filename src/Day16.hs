module Day16 where

import Text.Parsec
import Control.Monad

type Compound = (String, Int)
type CompoundReading = String -> (Int -> Int -> Bool)
data Sue = Sue Int [Compound] deriving (Show)

isCorrectSue :: CompoundReading -> Sue -> Bool
isCorrectSue r (Sue _ cs) = all compoundMatches detectedCompounds
  where compoundMatches (k, v) = maybe True (flip (r k) v) (lookup k cs)

detectedCompounds :: [Compound]
detectedCompounds =
  [ ("children", 3)
  , ("cats", 7)
  , ("samoyeds", 2)
  , ("pomeranians", 3)
  , ("akitas", 0)
  , ("vizslas", 0)
  , ("goldfish", 5)
  , ("trees", 3)
  , ("cars", 2)
  , ("perfumes", 1)
  ]

trueCompoundReading :: CompoundReading
trueCompoundReading = \case
  "cats"        -> (>)
  "trees"       -> (>)
  "pomeranians" -> (<)
  "goldfish"    -> (<)
  _             -> (==)

naiveCompoundReading :: CompoundReading
naiveCompoundReading _ = (==)

parseSues :: String -> [Sue]
parseSues s = ss
  where
    Right ss = parse sues "" s
    sues = sue `sepEndBy` endOfLine
    sue = do
      void $ string "Sue "
      n <- num
      void $ char ':'
      cs <- compound `sepBy` char ','
      return $ Sue n cs
    compound = do
      void $ many space
      prop <- many1 letter
      void $ string ": "
      n <- num
      return (prop, n)
    num = read <$> many1 digit