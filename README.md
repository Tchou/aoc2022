# aoc2022
Repository for the [advent of Code 2022](https://adventofcode.com/2022) in OCaml and Python.

## Dependencies and usage

Programs expect 1 argument on the command-line, the problem number of
the form `nna` or `nnb`, where `nn` is the two digit problem day
(between 01 and 25) and `a` or `b` indicates wheter to solve for part
1 or part 2. The program expects the input data on `stdin`.

### OCaml
Only the OCaml compiler and `dune` are required. Run with
```
$ cd ocaml/
$ dune exec --display=quiet -- bin/main.exe 11b < ../inputs/input_11.txt
```
The expected solution for the input is printed on `stdout`.

### Python
Only Python >= 3.8 is required

## Rationale

The code only uses the standard libraries. As much as possible I tried
to implement not too naïve solutions. Sometimes the first version of a
problem was an instance of a more general problem (often given in part
two), so the preliminary code that gave the solution for part one was
removed.
