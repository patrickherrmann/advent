module Day15 where

import Text.Parsec
import Control.Monad

data Ingredient = Ingredient String [Int] Int deriving (Show)

parseIngredients :: String -> [Ingredient]
parseIngredients s = is
  where
    (Right is) = parse ingredients "" s
    ingredients = ingredient `sepEndBy` endOfLine
    ingredient = do
      n <- name
      void $ string ": capacity "
      cap <- num
      void $ string ", durability "
      dur <- num
      void $ string ", flavor "
      flav <- num
      void $ string ", texture "
      texture <- num
      void $ string ", calories "
      cals <- num
      return $ Ingredient n [cap, dur, flav, texture] cals
    name = many1 letter
    num = do
      sign <- optionMaybe $ char '-'
      nat <- many1 digit
      let n = maybe nat (:nat) sign
      return $ read n