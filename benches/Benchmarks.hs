module Main (main) where

import Criterion.Main (bgroup, defaultMain)
import qualified Benchmarks.Day04

main :: IO ()
main = defaultMain
  [ bgroup "Day04" Benchmarks.Day04.benchmarks
  ]