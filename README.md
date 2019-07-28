# sequence-alignment-haskell

This project contains an implementation of a dynamic programming algorithm for
sequence alignment in Haskell.

The module `Fib` contains a simple example of dynamic programming in Haskell.
The module consists of three functions which compute Fibonacci numbers. One
function uses recursion and the other two functions use dynamic programming; one
is based on functional arrays and the other is based on the state monad. Note
that time and memory for evaluation can be checked in GHCi with the `:set +s`
option.

The module `Align` contains the implementation of the sequence alignment
algorithm. The algorithm is adapted from the textbook *Algorithm Design* by Jon
Kleinberg and Eva Tardos. The function `opt` computes the minimal alignment
costs and the function `sol` finds the optimal alignment.

## Instructions

We will assume that this project exists locally and we are logged into a shell
where the working directory is the root of the project.

This project uses [Stack][stack] to simplify dependency management.

### Build

Build the project with the command `stack build`.

### Test

Test the `Fib` and `Align` modules with the command `stack test`.

### GHCi

Run GHCi with the command `stack ghci`. In GHCi, load the `Align` module with
the `:m Align` option.

### Run App

Suppose we want to compute an alignment of human and hamster DNA. We can do this
by running the following command
```
$ stack exec main-exe -- res/human.fasta res/hamster.fasta
```
Note that this command might take a minute or so to complete.

[stack]: https://www.haskellstack.org
