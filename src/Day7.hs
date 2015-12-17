module Day7 where

import Control.Applicative hiding ((<|>))
import Data.Word
import Data.Bits
import Text.Parsec hiding (State)
import Text.Parsec.String
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy

type Instruction = (Ident, Expr)

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

findSignalAtA :: String -> Signal
findSignalAtA s = evalState (evalIdent "a") $ parseEnv s

findSignalAtAWithModifiedB :: String -> Signal
findSignalAtAWithModifiedB s = evalState (evalIdent "a") env'
  where
    env = parseEnv s
    env' = Map.insert "b" (Atom (Lit 956)) env

evalIdent :: Ident -> State Env Signal
evalIdent i = lookupIdent i >>= eval

eval :: Expr -> State Env Signal
eval (Atom a) = evalAtom a
eval (Or a b) = (.|.) <$> evalAtom a <*> evalAtom b
eval (And a b) = (.&.) <$> evalAtom a <*> evalAtom b
eval (LShift i b) = (`shift` b) <$> evalIdent i
eval (RShift i b) = (`shift` (-b)) <$> evalIdent i
eval (Not a) = complement <$> evalAtom a

evalAtom :: Atom -> State Env Signal
evalAtom (Lit s) = return s
evalAtom (Ref i) = do
  s <- evalIdent i
  memoizeIdent i s
  return s

lookupIdent :: Ident -> State Env Expr
lookupIdent i = gets (! i)

memoizeIdent :: Ident -> Signal -> State Env ()
memoizeIdent i s = modify (Map.insert i (Atom (Lit s)))

-- Parsing AST

parseEnv :: String -> Env
parseEnv s = Map.fromList is
  where (Right is) = parse instructions "" s

instructions :: Parser [Instruction]
instructions = instruction `sepEndBy` endOfLine

instruction :: Parser Instruction
instruction = do
  e <- expr
  string " -> "
  i <- ident
  return (i, e)

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