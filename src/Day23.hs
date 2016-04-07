module Day23 where

import Text.Parsec hiding (State)

type Offset = Int

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

parseInstructions :: String -> [Instruction]
parseInstructions s = is
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
