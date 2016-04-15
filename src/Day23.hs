{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Day23 where

import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Vector as V
import Text.Parsec hiding (State)

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

executeProgram :: Machine -> Program -> Machine
executeProgram m p = execState (runProgram p) m

runProgram :: Program -> State Machine ()
runProgram p = do
  i <- use pc
  case p V.!? i of
    Nothing -> return ()
    Just instr -> do
      runInstruction instr
      runProgram p

runInstruction :: Instruction -> State Machine ()
runInstruction = \case
  Hlf r -> reg r %= (`div` 2) >> next
  Tpl r -> reg r *= 3 >> next
  Inc r -> reg r += 1 >> next
  Jmp o -> jumpBy o
  Jie r o -> branch r even o
  Jio r o -> branch r (==1) o

branch :: Register -> (Int -> Bool) -> Int -> State Machine ()
branch r p o = do
  rv <- use (reg r)
  if p rv
    then jumpBy o
    else next

jumpBy :: Int -> State Machine ()
jumpBy o = pc += o

next :: State Machine ()
next = pc += 1

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

initialMachine2 :: Machine
initialMachine2 = Machine
  { _ra = 1
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
