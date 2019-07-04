module Main where

import           Runner
import           Solver

main :: IO ()
main = do
  putStrLn "Dec 1"
  run2 solveA solveB
