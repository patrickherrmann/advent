module Day7 where

import Control.Applicative hiding ((<|>))
import Data.Word
import Text.Parsec
import Text.Parsec.String
import qualified Data.Map.Strict as Map

type AST = [Command]

data Command = Command Expr Ident deriving (Show)

data Expr
  = Atom Atom
  | And Atom Atom
  | Or Atom Atom
  | LShift Ident Int
  | RShift Ident Int
  | Not Atom
  deriving (Show)

data Atom
  = Lit Signal
  | Ref Ident
  deriving (Show)

type Ident = String
type Signal = Word16

type Env = Map.Map Ident Expr

-- Interpret AST


-- Parsing AST

parseAST :: String -> AST
parseAST s = cs
  where (Right cs) = parse commands "" s

commands :: Parser [Command]
commands = command `sepEndBy` endOfLine

command :: Parser Command
command = do
  e <- expr
  string " -> "
  i <- ident
  return $ Command e i

expr :: Parser Expr
expr = try andExpr
   <|> try orExpr
   <|> try lShiftExpr
   <|> try rShiftExpr
   <|> try notExpr
   <|> atomExpr

atomExpr :: Parser Expr
atomExpr = Atom <$> atom

atom :: Parser Atom
atom = try litAtom <|> refAtom

litAtom :: Parser Atom
litAtom = Lit <$> (read <$> many1 digit)

refAtom :: Parser Atom
refAtom = Ref <$> ident

andExpr :: Parser Expr
andExpr = do
  x <- atom
  string " AND "
  y <- atom
  return $ And x y

orExpr :: Parser Expr
orExpr = do
  x <- atom
  string " OR "
  y <- atom
  return $ Or x y

lShiftExpr :: Parser Expr
lShiftExpr = do
  x <- ident
  string " LSHIFT "
  bs <- num
  return $ LShift x bs

rShiftExpr :: Parser Expr
rShiftExpr = do
  x <- ident
  string " RSHIFT "
  bs <- num
  return $ RShift x bs

notExpr :: Parser Expr
notExpr = Not <$> (string "NOT " *> atom)

num :: Parser Int
num = read <$> many1 digit

ident :: Parser Ident
ident = many1 lower