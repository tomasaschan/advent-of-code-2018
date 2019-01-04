module Solvers where

import qualified Aoc2017.Dec1
import qualified Solvers.Dec1 as Dec1
import qualified Solvers.Dec2 as Dec2
import qualified Solvers.Dec3 as Dec3
import qualified Solvers.Dec4 as Dec4
import qualified Solvers.Dec5 as Dec5
import qualified Solvers.Dec6 as Dec6
import qualified Solvers.Dec7 as Dec7
import qualified Solvers.Dec8 as Dec8
import qualified Solvers.Dec9 as Dec9
import qualified Solvers.Dec10 as Dec10
import qualified Solvers.Dec11 as Dec11
import qualified Solvers.Dec12 as Dec12
import qualified Solvers.Dec13 as Dec13
import qualified Solvers.Dec14 as Dec14
import qualified Solvers.Dec18 as Dec18

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
solver "7a" = Just Dec7.solveA
solver "7b" = Just $ Dec7.solveB 5 60
solver "8a" = Just Dec8.solveA
solver "8b" = Just Dec8.solveB
solver "9a" = Just Dec9.solveA
solver "9b" = Just Dec9.solveB
solver "10" = Just Dec10.solve
solver "11a" = Just Dec11.solveA
solver "11b" = Just Dec11.solveB
solver "12a" = Just Dec12.solveA
solver "12b" = Just Dec12.solveB
solver "13a" = Just Dec13.solveA
solver "13b" = Just Dec13.solveB
solver "14a" = Just Dec14.solveA
solver "18a" = Just Dec18.solveA
solver "18b" = Just Dec18.solveB
solver _ = Nothing
