This package contains my solutions to the puzzles at [adventofcode.com](http://adventofcode.com).

Each day of the challenge has its own module. There is no `main` function; load up a REPL with `stack init ghci` and import the module from any day to execute its functions. All of the my inputs are available in the `inputs` directory.

For example, to solve both parts of the first puzzle, you can execute:

```
$ stack exec ghci
λ> :m Day1
λ> day1input <- readFile "inputs/Day1.txt"
λ> finalFloor day1input
74
λ> position day1input
1795

```