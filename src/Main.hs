import Day1
import Prelude hiding (floor)

main :: IO ()
main = do
  input <- readFile "inputs/Day1.txt"
  print $ position input