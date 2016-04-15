module Day13 where

import Control.Monad
import Day09 hiding (parseEdges, pathDistance)
import qualified Data.Map as Map
import Text.Parsec

maximumWithoutPat :: String -> Int
maximumWithoutPat = maximumHappiness . constructGraph . parseEdges

maximumWithPat :: String -> Int
maximumWithPat = maximumHappiness . addPat . constructGraph . parseEdges

maximumHappiness :: Graph -> Int
maximumHappiness = maximum . map snd . measuredPaths happinessChange

happinessChange :: EdgeMap -> Path -> Int
happinessChange es = sum . map cost . pairs . closed
  where
    closed ns@(n:_) = ns ++ [n]
    pairs ns = zip ns (tail ns)
    cost (a, b) = (es Map.! (a, b)) + (es Map.! (b, a))

addPat :: Graph -> Graph
addPat (Graph ns es) = Graph (pat:ns) es'
  where
    newEs n = [((pat, n), 0), ((n, pat), 0)]
    es' = es `Map.union` Map.fromList (ns >>= newEs)
    pat = "Pat"

parseEdges :: String -> [Edge]
parseEdges s = es
  where
    Right es = parse edges "" s
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
