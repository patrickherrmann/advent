module Benchmarks.Day04 (benchmarks) where

import Criterion (Benchmark, bench, nf)
import Day04

benchmarks :: [Benchmark]
benchmarks =
  [ bench "findAdventCoin"
      $ nf (findAdventCoin "ckczppom") 5
  ]