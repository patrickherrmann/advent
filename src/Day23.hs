{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Day23 where

import Control.Lens
import Control.Monad.State.Strict
import Text.Parsec hiding (State)
import qualified Data.Vector as V

type Offset = Int
type Program = V.Vector Instruction

data Instruction
  = Hlf Register
  | Tpl Register
  | Inc Register
  | Jmp Offset
  | Jie Register Offset
  | Jio Register Offset
  deriving (Show)

data Register
  = A
  | B
  deriving (Show, Eq)

data Machine = Machine
  { _ra :: Int
  , _rb :: Int
  , _pc :: Int
  }
  deriving (Show)

makeLenses ''Machine

executeProgram :: Program -> Machine
executeProgram p = execState (runProgram p) initialMachine

runProgram :: Program -> State Machine ()
runProgram p = do
  pci <- use pc
  case p V.!? pci of
    Nothing -> return ()
    Just i -> do
      runInstruction pci i
      runProgram p

runInstruction :: Int -> Instruction -> State Machine ()
runInstruction i = \case
  Hlf r -> reg r %= (`div` 2) >> pc += 1
  Tpl r -> reg r *= 3 >> pc += 1
  Inc r -> reg r += 1 >> pc += 1
  Jmp o -> pc .= i + o
  Jie r o -> do
    rv <- use (reg r)
    if even rv
      then pc .= i + o
      else pc += 1
  Jio r o -> do
    rv <- use (reg r)
    if rv == 1
      then pc .= i + o
      else pc += 1

reg :: Register -> Lens' Machine Int
reg = \case
  A -> ra
  B -> rb

initialMachine :: Machine
initialMachine = Machine
  { _ra = 0
  , _rb = 0
  , _pc = 0
  }

parseProgram :: String -> Program
parseProgram s = V.fromList is
  where
    Right is = parse instructions "" s
    instructions = instruction `sepBy` endOfLine
    instruction = try parseHlf
              <|> try parseTpl
              <|> try parseInc
              <|> try parseJmp
              <|> try parseJie
              <|> parseJio
    parseHlf = Hlf <$> (string "hlf " *> parseRegister)
    parseTpl = Tpl <$> (string "tpl " *> parseRegister)
    parseInc = Inc <$> (string "inc " *> parseRegister)
    parseJmp = Jmp <$> (string "jmp " *> parseOffset)
    parseJie = Jie <$> (string "jie " *> parseRegister) <*> (string ", " *> parseOffset)
    parseJio = Jio <$> (string "jio " *> parseRegister) <*> (string ", " *> parseOffset)
    parseRegister = try parseA <|> parseB
    parseA = string "a" *> return A
    parseB = string "b" *> return B
    parseOffset= (*) <$> (try parseMinus <|> parsePlus) <*> (read <$> many1 digit)
    parseMinus = string "-" *> return (-1)
    parsePlus = string "+" *> return 1
