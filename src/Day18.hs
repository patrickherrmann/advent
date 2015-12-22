module Day18 where

import Data.Maybe
import Control.Monad
import Data.Map ((!))
import qualified Data.Map as Map

type Coord = (Int, Int)
type Grid = Map.Map (Int, Int) Bool

lightsOn :: Grid -> Int
lightsOn = length . filter id . Map.elems

gridSequence :: Grid -> [Grid]
gridSequence = iterate nextGrid

nextGrid :: Grid -> Grid
nextGrid g = Map.fromList assocs
  where assocs = map (\c -> (c, newState g c)) coords

newState :: Grid -> Coord -> Bool
newState g c = shouldStayOn || shouldTurnOn
  where
    currentState = g ! c
    shouldStayOn = currentState && neighborsActive `elem` [2, 3]
    shouldTurnOn = not currentState && neighborsActive == 3
    neighborsActive = length $ filter id $ map (g !) $ neighbors c

neighbors :: Coord -> [Coord]
neighbors (x, y) = do
  x' <- [x - 1, x, x + 1]
  y' <- [y - 1, y, y + 1]
  let notSelf = x /= x' || y /= y'
  let onGrid = x' > 0 && y' > 0 && x' <= 100 && y' <= 100
  guard (notSelf && onGrid)
  return (x', y')

parseInitialGrid :: String -> Grid
parseInitialGrid s = Map.fromList $ zip coords states
  where
    states = catMaybes $ charState <$> s
    charState '.' = Just False
    charState '#' = Just True
    charState _ = Nothing

coords :: [Coord]
coords = [(x, y) | x <- [1..100], y <- [1..100]]