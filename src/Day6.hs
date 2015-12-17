module Day6 where

import Data.Foldable
import Control.Applicative hiding ((<|>))
import qualified Data.Map.Strict as Map
import Text.Parsec
import Text.Parsec.String

type Grid = Map.Map Coord Status

type Coord = (Int, Int)

data Status = On | Off deriving (Eq)

data Command = Command Action Range

data Action
  = TurnOn
  | TurnOff
  | Toggle

data Range = Range Coord Coord

countLightsAfterCommands :: String -> Int
countLightsAfterCommands s = lightsOn fg
  where
    cs = parseCommands s
    fg = foldl' performCommand createGrid cs

createGrid :: Grid
createGrid = Map.fromList $ zip coords $ repeat Off
  where coords = [(x, y) | x <- [0..999], y <- [0..999]]

performCommand :: Grid -> Command -> Grid
performCommand grid (Command a r) = foldl' (flip act) grid cs
  where
    act = Map.adjust (performAction a)
    cs = coordsInRange r

lightsOn :: Grid -> Int
lightsOn = length . filter (== On) . Map.elems

performAction :: Action -> Status -> Status
performAction Toggle On = Off
performAction Toggle Off = On
performAction TurnOn _ = On
performAction TurnOff _ = Off

coordsInRange :: Range -> [Coord]
coordsInRange (Range (lx, ly) (hx, hy)) =
  [(x, y) | x <- [lx..hx], y <- [ly..hy]]

insideRange :: Range -> Coord -> Bool
insideRange (Range (lx, ly) (hx, hy)) (x, y) =
  x >= lx &&
  y >= ly &&
  x <= hx &&
  y <= hy

-- Parsing commands

parseCommands :: String -> [Command]
parseCommands s = cs
  where (Right cs) = parse commands "" s

commands :: Parser [Command]
commands = command `sepBy` endOfLine

command :: Parser Command
command = do
  a <- action
  space
  r <- range
  return $ Command a r

action :: Parser Action
action = try turnOn <|> try turnOff <|> toggle
  where
    turnOn = string "turn on" *> return TurnOn
    turnOff = string "turn off" *> return TurnOff
    toggle = string "toggle" *> return Toggle

range :: Parser Range
range = do
  lo <- coord
  string " through "
  hi <- coord
  return $ Range lo hi

coord :: Parser Coord
coord = do
  x <- num
  char ','
  y <- num
  return (x, y)

num :: Parser Int
num = read <$> many1 digit