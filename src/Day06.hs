module Day06 where

import Control.Monad
import Control.Monad.ST
import Data.Array.MArray hiding (range)
import Data.Array.ST hiding (range)
import Text.Parsec

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

type Grid s = STUArray s Coord Status

type Status = Bool

countLightsAfterCommands :: String -> Int
countLightsAfterCommands s = runST $ do
  let cs = parseCommands s
  grid <- createGrid
  forM_ cs $ \c -> performCommand grid c
  lightsOn grid

createGrid ::ST s (Grid s)
createGrid = newArray ((0, 0), (999,999)) False

performCommand :: Grid s -> Command -> ST s ()
performCommand grid (Command a r) = do
  let cs = coordsInRange r
  forM_ cs $ \c -> do
    b <- readArray grid c
    writeArray grid c $ performAction a b

lightsOn :: Grid s -> ST s Int
lightsOn grid = do
  es <- getElems grid
  return $ length $ filter id es

performAction :: Action -> Status -> Status
performAction = \case
  Toggle  -> not
  TurnOn  -> const True
  TurnOff -> const False

-- Part 2

type Grid2 s = STUArray s Coord Brightness

type Brightness = Int

measureBrightnessAfterCommands :: String -> Int
measureBrightnessAfterCommands s = runST $ do
  let cs = parseCommands s
  grid <- createGrid2
  forM_ cs $ \c -> performCommand2 grid c
  gridBrightness grid

createGrid2 ::ST s (Grid2 s)
createGrid2 = newArray ((0, 0), (999,999)) 0

performCommand2 :: Grid2 s -> Command -> ST s ()
performCommand2 grid (Command a r) = do
  let cs = coordsInRange r
  forM_ cs $ \c -> do
    b <- readArray grid c
    writeArray grid c $ performAction2 a b

gridBrightness :: Grid2 s -> ST s Int
gridBrightness grid = sum <$> getElems grid

performAction2 :: Action -> Brightness -> Brightness
performAction2 = curry $ \case
  (Toggle, b)  -> b + 2
  (TurnOn, b)  -> b + 1
  (TurnOff, 0) -> 0
  (TurnOff, b) -> b - 1

-- Parsing commands

parseCommands :: String -> [Command]
parseCommands s = cs
  where
    (Right cs) = parse commands "" s
    commands = command `sepEndBy` endOfLine
    command = Command <$> action <*> (space *> range)
    action = try turnOn <|> try turnOff <|> toggle
    turnOn = string "turn on" *> return TurnOn
    turnOff = string "turn off" *> return TurnOff
    toggle = string "toggle" *> return Toggle
    range = Range <$> coord <*> (string " through " *> coord)
    coord = (,) <$> num <*> (char ',' *> num)
    num = read <$> many1 digit