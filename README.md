# :christmas_tree: :santa: Advent of Code :santa: :christmas_tree:

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

Some solutions are overengineered, others underengineered, depending on how interesting I found the problem. You may need to assemble the solution yourself (example below), but all of the pieces are there.

```
$ stack repl --no-load
λ> :l Day23
λ> p <- parseProgram <$> readFile "inputs/Day23.txt"
λ> executeProgram initialMachine p
Machine {_ra = 1, _rb = 255, _pc = 48}
```