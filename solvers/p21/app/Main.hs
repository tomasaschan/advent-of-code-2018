module Main where

import           Runner
import           Solver

main :: IO ()
main = do
  putStrLn "Dec 21"
  run1 "a" (solve a)

  -- run2 a b
