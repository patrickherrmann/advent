{-# LANGUAGE QuasiQuotes #-}

module Benchmarks.Day06 (benchmarks) where

import Text.Heredoc
import Criterion (Benchmark, bench, nf)
import Day06

benchmarks :: [Benchmark]
benchmarks =
  [ bench "countLightsAfterCommands"
      $ nf countLightsAfterCommands input
  , bench "measureBrightnessAfterCommands"
      $ nf measureBrightnessAfterCommands input
  ]

input = [there|./inputs/Day06.txt|]