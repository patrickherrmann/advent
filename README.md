This package contains my solutions to the puzzles at [adventofcode.com](http://adventofcode.com).

Each day of the challenge has its own module. There is no `main` function; load up a REPL with `stack repl --no-load` and load the module from any day to execute its functions. All of the my inputs are available in the `inputs` directory.

For example, to solve both parts of the first puzzle, you can execute:

```
$ stack repl --no-load
λ> :l Day01
λ> input <- readFile "inputs/Day01.txt"
λ> finalFloor input
74
λ> position input
1795

```