module Main (main) where

import Criterion.Main (bgroup, defaultMain)
import qualified Benchmarks.Day04
import qualified Benchmarks.Day06

main :: IO ()
main = defaultMain
  [ bgroup "Day04" Benchmarks.Day04.benchmarks
  , bgroup "Day06" Benchmarks.Day06.benchmarks
  ]