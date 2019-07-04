module Main where

import           Runner
import           Solver

main :: IO ()
main = do
  putStrLn "Dec PROBLEM"
  run2 solveA solveB
