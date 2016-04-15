module Day24 where

import Data.List
import Data.Monoid
import Data.Ord
import Data.Vector (Vector, snoc)
import qualified Data.Vector as V

data Group = Group
  { packages :: Vector Int
  , count :: Int
  , quantumEntanglement :: Int
  } deriving (Show)

estimateBestGroup :: Int -> Vector Int -> Group
estimateBestGroup n v = minimumBy (comparing count <> comparing quantumEntanglement) gs
  where gs = take 100000 $ map fst $ possibleGroups (sum v `div` n) $ V.reverse v

possibleGroups :: Int -> Vector Int -> [(Group, Vector Int)]
possibleGroups w v = map groupFirst $ go (V.empty, v)
  where
    groupFirst (a, bc) = (makeGroup a, bc)
    next yes (a, as) = (yes `snoc` a, as)
    go p@(yes, no) = case compare (sum yes) w of
      GT -> []
      EQ -> [p]
      LT -> next yes <$> removeEach no >>= go

makeGroup :: Vector Int -> Group
makeGroup v = Group
  { packages = v
  , count = length v
  , quantumEntanglement = product v
  }

removeEach :: Vector a -> [(a, Vector a)]
removeEach v = remove v <$> [0..length v - 1]

remove :: Vector a -> Int -> (a, Vector a)
remove v i = (v V.! i, V.slice 0 i v <> V.slice (i + 1) (length v - i - 1) v)

parseWeights :: String -> Vector Int
parseWeights s = V.fromList $ read <$> lines s