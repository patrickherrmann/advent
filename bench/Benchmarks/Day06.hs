module Benchmarks.Day06 (benchmarks) where

import Criterion (Benchmark, bench, nf)
import Day06
import Text.Heredoc

benchmarks :: [Benchmark]
benchmarks =
  [ bench "countLightsAfterCommands"
      $ nf countLightsAfterCommands input
  , bench "measureBrightnessAfterCommands"
      $ nf measureBrightnessAfterCommands input
  ]

input = [there|./inputs/Day06.txt|]