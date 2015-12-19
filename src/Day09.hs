module Day09 where

import Data.List
import Data.Ord
import Text.Parsec
import qualified Data.Map as Map

type Node = String
type Distance = Int
type Path = [Node]
type MeasuredPath = (Path, Distance)
type Edge = ((Node, Node), Distance)
type EdgeMap = Map.Map (Node, Node) Distance

data Graph = Graph [Node] EdgeMap deriving (Show)

parseGraph :: String -> Graph
parseGraph = constructGraph . addInverses . parseEdges

longestPath :: Graph -> MeasuredPath
longestPath = maximumBy (comparing snd) . measuredPaths pathDistance

shortestPath :: Graph -> MeasuredPath
shortestPath = minimumBy (comparing snd) . measuredPaths pathDistance

measuredPaths :: (EdgeMap -> Path -> Distance) -> Graph -> [MeasuredPath]
measuredPaths m (Graph ns es) = map (\p -> (p, m es p)) paths
  where paths = permutations ns

pathDistance :: EdgeMap -> Path -> Int
pathDistance es p = sum $ map (es Map.!) pairs
  where pairs = zip p (tail p)

constructGraph :: [Edge] -> Graph
constructGraph es = Graph nodes edges
  where
    edges = Map.fromList es
    nodes = nub $ map fst $ Map.keys edges

addInverses :: [Edge] -> [Edge]
addInverses es = es ++ map flipEdge es
  where flipEdge ((a, b), d) = ((b, a), d)

parseEdges :: String -> [Edge]
parseEdges s = es
  where
    (Right es) = parse edges "" s
    edges = edge `sepEndBy` endOfLine
    edge = (,) <$> cityPair <*> (string " = " *> num)
    cityPair = (,) <$> city <*> (string " to " *> city)
    city = many1 letter
    num = read <$> many1 digit