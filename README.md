# Midterm 2 Problems (200 points)

The main topics for Midterm 2 are:

- higher order functions (Using them to write non recursive functions,
  implementing them for other kinds of ADTs)

- Nano (and variations of it), interpreters (which could involve changes to /
  variations on Nano), closures

- Semantics, evaluation strategy

You can run the test suite with `stack test` or `make`, like the PAs.

The general points breakdown is: 80 public, 80 private, 40 graded by hand.

## Problem 1: Folds for Tables (50 points)
Complete the 4 problems described in [FoldTables.hs](src/Language/Nano2/FoldTables.hs).

## Problem 2: One Step Reduction (70 points)
Complete the functions `isValue`, `subst`, and `reduce1` in
[Reduce.hs](src/Language/Nano2/Reduce.hs). The requirements are described in
the file.

All test cases rely on `reduce1` being completed - there are no test cases
that test only `isValue` or `subst`.
