module Benchmarks.Day04 (benchmarks) where

import Criterion (Benchmark, bench, nf)
import Day04
import Text.Heredoc

benchmarks :: [Benchmark]
benchmarks =
  [ bench "findAdventCoin5"
      $ nf (findAdventCoin input) 5
  , bench "findAdventCoin6"
      $ nf (findAdventCoin input) 6
  ]

input = [there|./inputs/Day04.txt|]