module Day13 where

import Day09 hiding (parseEdges, pathDistance)
import Text.Parsec
import Control.Monad
import Data.List
import Data.Ord
import Data.Map ((!))

maximumHappiness :: String -> Int
maximumHappiness s = d
  where
    g = constructGraph $ parseEdges s
    (_, d) = maximumBy (comparing snd) ps
    ps = measuredPaths happinessChange g

happinessChange :: EdgeMap -> Path -> Int
happinessChange es = sum . map cost . pairs . closed
  where
    closed ns@(n:_) = ns ++ [n]
    pairs ns = zip ns (tail ns)
    cost (a, b) = (es ! (a, b)) + (es ! (b, a))

parseEdges :: String -> [Edge]
parseEdges s = es
  where
    (Right es) = parse edges "" s
    edges = edge `sepEndBy` endOfLine
    edge = do
      a <- person
      void $ string " would "
      sign <- try gain <|> lose
      void $ string " "
      h <- num
      void $ string " happiness units by sitting next to "
      b <- person
      void $ string "."
      return ((a, b), sign * h)
    gain = string "gain" *> pure 1
    lose = string "lose" *> pure (-1)
    person = many1 letter
    num = read <$> many1 digit
