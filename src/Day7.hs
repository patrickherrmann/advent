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

findSignalAtA :: String -> Signal
findSignalAtA s = evalState (evalIdent "a") $ parseEnv s

findSignalAtAWithModifiedB :: String -> Signal
findSignalAtAWithModifiedB s = evalState (evalIdent "a") env'
  where
    env = parseEnv s
    env' = Map.insert "b" (Atom (Lit 956)) env

-- Memoized evaluation of the environment

evalIdent :: Ident -> State Env Signal
evalIdent i = lookupIdent i >>= evalExpr

evalExpr :: Expr -> State Env Signal
evalExpr (Atom a) = evalAtom a
evalExpr (Or a b) = (.|.) <$> evalAtom a <*> evalAtom b
evalExpr (And a b) = (.&.) <$> evalAtom a <*> evalAtom b
evalExpr (LShift i b) = (`shift` b) <$> evalIdent i
evalExpr (RShift i b) = (`shift` (-b)) <$> evalIdent i
evalExpr (Not a) = complement <$> evalAtom a

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
  where
    (Right is) = parse instructions "" s
    instructions = instruction `sepEndBy` endOfLine
    instruction :: Parser Instruction
    instruction = do
      e <- expr
      void $ string " -> "
      i <- ident
      return (i, e)
    expr = try andExpr
       <|> try orExpr
       <|> try lShiftExpr
       <|> try rShiftExpr
       <|> try notExpr
       <|> atomExpr
    atomExpr = Atom <$> atom
    atom = try litAtom <|> refAtom
    litAtom = Lit <$> (read <$> many1 digit)
    refAtom = Ref <$> ident
    andExpr = And <$> atom <*> (string " AND " *> atom)
    orExpr = Or <$> atom <*> (string " OR " *> atom)
    lShiftExpr = LShift <$> ident <*> (string " LSHIFT " *> num)
    rShiftExpr = RShift <$> ident <*> (string " RSHIFT " *> num)
    notExpr = Not <$> (string "NOT " *> atom)
    num = read <$> many1 digit
    ident = many1 lower