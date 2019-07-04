module Main where

import           Runner
import           Solver

main :: IO ()
main = do
  putStrLn "Dec 7"
  run2 solveA (solveB 5 60)
