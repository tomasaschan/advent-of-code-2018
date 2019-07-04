module Main where

import           Runner
import           Solver

main :: IO ()
main = do
  putStrLn "Dec 6"
  run2 solveA (solveB 10000)
