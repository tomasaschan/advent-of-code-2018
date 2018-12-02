module Main where

import System.Environment
import Data.List
import Solvers
import Utils

main :: IO ()
main = do
  args <- getArgs
  answer <- liftSolver Solvers.solver (parseArgs args)
  putStrLn ("Solving " ++ (intercalate " with input " args) ++ "\n" ++ answer)