module Solvers where

import Text.Printf
import qualified Aoc2017.Dec1
import qualified Dec1
import qualified Dec2
import qualified Dec3
import qualified Dec5

solver :: String -> Maybe ([String] -> String)
solver "2017-1a" = Just Aoc2017.Dec1.solveA
solver "2017-1b" = Just Aoc2017.Dec1.solveB
solver "1a" = Just Dec1.solveA
solver "1b" = Just Dec1.solveB
solver "2a" = Just Dec2.solveA
solver "2b" = Just Dec2.solveB
solver "3a" = Just Dec3.solveA
solver "3b" = Just Dec3.solveB
solver "5a" = Just Dec5.solveA
solver "5b" = Just Dec5.solveB
solver _ = Nothing
