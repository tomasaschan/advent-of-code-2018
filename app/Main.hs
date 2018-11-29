module Main where

import System.Environment
import Data.List
import Text.Printf
import Solvers
import System.IO
import System.Directory

readSafe :: FilePath -> Bool -> IO (Maybe [String])
readSafe input True = do
  content <- readFile input
  return (Just (lines content))
readSafe _ False = do
  return Nothing

readInput :: FilePath -> IO (Maybe [String])
readInput input = do
  exists <- doesFileExist input
  readSafe input exists

parseArgs :: [String] -> Maybe (String, String)
parseArgs [problem, inputFile] = Just (problem, inputFile)
parseArgs _ = Nothing

solve :: Maybe ([String] -> String) -> Maybe [String] -> String
solve (Just solver) (Just input) = solver input
solve _ Nothing = "Could not find input file."
solve Nothing _ = "Could not find solver."

liftSolver :: (String -> Maybe ([String] -> String)) -> (Maybe (String, String)) -> IO String
liftSolver solvers (Just (problem, inputFile)) = do
  input <- readInput inputFile
  return $ solve (solvers problem) input
liftSolver _ Nothing = do
  return "Usage: aoc <problem> <input-file>"

main :: IO ()
main = do
  args <- getArgs
  answer <- liftSolver Solvers.solver (parseArgs args)
  putStrLn ("Solving " ++ (intercalate " with input " args) ++ "\n" ++ answer)