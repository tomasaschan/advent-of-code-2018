module Solvers where

import qualified Aoc2017.Dec1
import qualified Dec1
import qualified Dec2
import qualified Dec3
import qualified Dec4
import qualified Dec5
import qualified Dec6
import qualified Dec8

solver :: String -> Maybe ([String] -> String)
solver "2017-1a" = Just Aoc2017.Dec1.solveA
solver "2017-1b" = Just Aoc2017.Dec1.solveB
solver "1a" = Just Dec1.solveA
solver "1b" = Just Dec1.solveB
solver "2a" = Just Dec2.solveA
solver "2b" = Just Dec2.solveB
solver "3a" = Just Dec3.solveA
solver "3b" = Just Dec3.solveB
solver "4a" = Just Dec4.solveA
solver "4b" = Just Dec4.solveB
solver "5a" = Just Dec5.solveA
solver "5b" = Just Dec5.solveB
solver "6a" = Just Dec6.solveA
solver "6b" = Just $ Dec6.solveB 10000
solver "8a" = Just Dec8.solveA
solver "8b" = Just Dec8.solveB
solver _ = Nothing
