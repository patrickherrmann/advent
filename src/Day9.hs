module Day9 where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import Text.Parsec
import Text.Parsec.String
import qualified Data.Map as Map

type City = String
type Distance = Int
type Path = [City]
type Edge = ((City, City), Distance)

data Graph = Graph [City] (Map.Map (City, City) Distance)

solution :: String -> (Path, Distance)
solution = shortestPath . constructGraph . parseEdges

shortestPath :: Graph -> (Path, Distance)
shortestPath g = minimumBy (comparing snd) ps'
  where
    ps = paths g
    ps' = map (\p -> (p, pathDistance g p)) ps

pathDistance :: Graph -> Path -> Int
pathDistance (Graph _ es) p = sum $ map (es Map.!) pairs
  where pairs = zip p (tail p)

paths :: Graph -> [Path]
paths (Graph cs _) = permutations cs

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