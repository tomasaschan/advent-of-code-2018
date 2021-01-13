module Main where

import           Runner
import           Solver

main :: IO ()
main = do
  putStrLn "Dec 21"
  run2 (solve a) (solve b)
