module Day18 where

import Control.Monad
import Data.Map ((!))
import Data.Maybe
import qualified Data.Map as Map

type Coord = (Int, Int)
type Grid = Map.Map (Int, Int) Bool
type GridRules = Grid -> Coord -> Bool

lightsOn :: Grid -> Int
lightsOn = length . filter id . Map.elems

nextGrid :: GridRules -> Grid -> Grid
nextGrid rules g = Map.fromList assocs
  where assocs = map (\c -> (c, rules g c)) coords

fixedCorners :: GridRules
fixedCorners g c@(x, y)
  | (x == 1 || x == 100) && (y == 1 || y == 100) = True
  | otherwise = standardRules g c

standardRules :: Grid -> Coord -> Bool
standardRules g c = shouldStayOn || shouldTurnOn
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