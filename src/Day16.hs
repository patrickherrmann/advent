module Day16 where

import Text.Parsec
import Control.Monad

type Compound = (String, Int)
data Sue = Sue Int [Compound]

parseSues :: String -> [Sue]
parseSues s = ss
  where
    (Right ss) = parse sues "" s
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
      count <- num
      return (prop, count)
    num = read <$> many1 digit