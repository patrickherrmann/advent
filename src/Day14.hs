module Day14 where

import Control.Monad
import Data.List
import Text.Parsec

data Reindeer = Reindeer String Int Int Int deriving (Show)

winningDistance :: [Reindeer] -> Int -> Int
winningDistance rs t = maximum $ map (!! t) $ reindeerDistanceGraphs rs

winningPoints :: [Reindeer] -> Int -> Int
winningPoints rs t = maximum points
  where
    points = map sum $ pointsGraphs graphs
    graphs = map (take t) $ reindeerDistanceGraphs rs

pointsGraphs :: [[Int]] -> [[Int]]
pointsGraphs = scores
  where
    scores = transpose . map points . transpose
    points g = let m = maximum g in map (score m) g
    score m s
      | m == s = 1
      | otherwise = 0

reindeerDistanceGraphs :: [Reindeer] -> [[Int]]
reindeerDistanceGraphs = map reindeerDistanceGraph

reindeerDistanceGraph :: Reindeer -> [Int]
reindeerDistanceGraph (Reindeer _ speed burst rest) = ds
  where
    ss = cycle $ replicate burst speed ++ replicate rest 0
    ds = zipWith (+) ss (0 : ds)

parseReindeer :: String -> [Reindeer]
parseReindeer s = rs
  where
    Right rs = parse reindeers "" s
    reindeers = reindeer `sepEndBy` endOfLine
    reindeer = do
      n <- name
      void $ string " can fly "
      speed <- num
      void $ string " km/s for "
      burst <- num
      void $ string " seconds, but then must rest for "
      rest <- num
      void $ string " seconds."
      return $ Reindeer n speed burst rest
    name = many1 letter
    num = read <$> many1 digit