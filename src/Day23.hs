module Day23 where

import Text.Parsec hiding (State)
import Text.Parsec.String

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
    instruction :: Parser Instruction
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
    parseJie = do
      string "jie "
      r <- parseRegister
      string ", "
      o <- parseOffset
      return $ Jie r o
    parseJio = do
      string "jio "
      r <- parseRegister
      string ", "
      o <- parseOffset
      return $ Jio r o
    parseRegister = try parseA <|> parseB
    parseA = string "a" *> return A
    parseB = string "b" *> return B
    parseOffset= do
      sign <- try parseMinus <|> parsePlus
      int <- read <$> many1 digit
      return $ sign * int
    parseMinus = string "-" *> return (-1)
    parsePlus = string "+" *> return 1
