# Advent of Code 2018

These are my solutions to [Advent of Code 2018][aoc].

It's the first time I do anything in Haskell, so feel free to
review the code and see if I can do something better!

[aoc]: https://adventofcode.com/2018

To set up, simply `stack setup`. Then you can run the tests
with `stack build --test` or run an actual solver with
`stack build --exec "aoc <problem> <input-file>"` (so, for
example `stack build --exec "aoc 1a ./input/dec1.txt"` for the
first problem).

In my workflow, I usually run the following command, to have
the tests and the solution I'm working on run automatically
on each file change:

    stack build --test --fast --file-watch --exec "aoc <problem> <input-file>"
