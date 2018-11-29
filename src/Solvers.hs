module Solvers where

import Text.Printf
import qualified Aoc2017.Dec1

solver :: String -> Maybe ([String] -> String)
solver "2017-1a" = Just Aoc2017.Dec1.solveA
solver "2017-1b" = Just Aoc2017.Dec1.solveB
solver _ = Nothing
