module Day09 where

import Data.List
import Data.Ord
import Text.Parsec
import qualified Data.Map as Map

type City = String
type Distance = Int
type Path = [City]
type MeasuredPath = (Path, Distance)
type Edge = ((City, City), Distance)
type EdgeMap = Map.Map (City, City) Distance

data Graph = Graph [City] EdgeMap

parseGraph :: String -> Graph
parseGraph = constructGraph . parseEdges

longestPath :: Graph -> MeasuredPath
longestPath = maximumBy (comparing snd) . measuredPaths

shortestPath :: Graph -> MeasuredPath
shortestPath = minimumBy (comparing snd) . measuredPaths

measuredPaths :: Graph -> [MeasuredPath]
measuredPaths (Graph cs es) = map (\p -> (p, pathDistance es p)) ps
  where ps = permutations cs

pathDistance :: EdgeMap -> Path -> Int
pathDistance es p = sum $ map (es Map.!) pairs
  where pairs = zip p (tail p)

constructGraph :: [Edge] -> Graph
constructGraph es = Graph cities edges
  where
    edges = Map.fromList $ es ++ map flipEdge es
    flipEdge ((a, b), d) = ((b, a), d)
    cities = nub $ map fst $ Map.keys edges

parseEdges :: String -> [Edge]
parseEdges s = es
  where
    (Right es) = parse edges "" s
    edges = edge `sepEndBy` endOfLine
    edge = (,) <$> cityPair <*> (string " = " *> num)
    cityPair = (,) <$> city <*> (string " to " *> city)
    city = many1 letter
    num = read <$> many1 digit