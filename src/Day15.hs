module Day15 where

import Text.Parsec
import Control.Monad
import Data.List

data Ingredient = Ingredient String [Int] deriving (Show)

scores :: [Ingredient] -> [(Int, Int)]
scores is = map score combos
  where combos = map (`zip` is) (recipes $ length is)

recipes :: Int -> [[Int]]
recipes n = filter ((==100) . sum) $ replicateM n [0..100]

score :: [(Int, Ingredient)] -> (Int, Int)
score = nutrition . map scoreStat . transpose . map stats
  where nutrition ss = (product $ init ss, last ss)

stats :: (Int, Ingredient) -> [Int]
stats (n, (Ingredient _ ps)) = map (*n) ps

scoreStat :: [Int] -> Int
scoreStat = atLeastZero . sum
  where
    atLeastZero x
      | x < 0 = 0
      | otherwise = x

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
      return $ Ingredient n [cap, dur, flav, texture, cals]
    name = many1 letter
    num = do
      sign <- optionMaybe $ char '-'
      nat <- many1 digit
      let n = maybe nat (:nat) sign
      return $ read n