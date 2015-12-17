module Day6 where

import Data.Foldable (foldl')
import Control.Applicative hiding ((<|>))
import qualified Data.Map.Strict as Map
import Text.Parsec
import Text.Parsec.String

data Command = Command Action Range

data Action
  = TurnOn
  | TurnOff
  | Toggle

data Range = Range Coord Coord

type Coord = (Int, Int)

coords :: [Coord]
coords = [(x, y) | x <- [0..999], y <- [0..999]]

coordsInRange :: Range -> [Coord]
coordsInRange (Range (lx, ly) (hx, hy)) =
  [(x, y) | x <- [lx..hx], y <- [ly..hy]]

-- Part 1

type Grid = Map.Map Coord Status

data Status = On | Off deriving (Eq)

countLightsAfterCommands :: String -> Int
countLightsAfterCommands s = lightsOn fg
  where
    cs = parseCommands s
    fg = foldl' performCommand createGrid cs

createGrid :: Grid
createGrid = Map.fromList $ zip coords $ repeat Off

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

-- Part 2

type Grid2 = Map.Map Coord Brightness

type Brightness = Int

measureBrightnessAfterCommands :: String -> Int
measureBrightnessAfterCommands s = totalBrightness fg
  where
    cs = parseCommands s
    fg = foldl' performCommand2 createGrid2 cs

createGrid2 :: Grid2
createGrid2 = Map.fromList $ zip coords $ repeat 0

performCommand2 :: Grid2 -> Command -> Grid2
performCommand2 grid (Command a r) = foldl' (flip act) grid cs
  where
    act = Map.adjust (performAction2 a)
    cs = coordsInRange r

totalBrightness :: Grid2 -> Int
totalBrightness = sum . Map.elems

performAction2 :: Action -> Brightness -> Brightness
performAction2 Toggle b = b + 2
performAction2 TurnOn b = b + 1
performAction2 TurnOff 0 = 0
performAction2 TurnOff b = b - 1

-- Parsing commands

parseCommands :: String -> [Command]
parseCommands s = cs
  where (Right cs) = parse commands "" s

commands :: Parser [Command]
commands = command `sepEndBy` endOfLine

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