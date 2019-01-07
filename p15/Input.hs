module Input (getInput) where

import CLI

import Control.Exception (try, IOException)
import Control.Monad.Except

import Solvers.Dec15

problem :: Options -> Problem
problem (Options { solvePart1 = True, solvePart2 = False }) = A
problem (Options { solvePart1 = False, solvePart2 = True }) = B
problem _ = Both

getSource :: Options -> ExceptT IOException IO [String]
getSource (Options { inputFrom = File path }) = either throwError return =<< liftIO (readFrom path)
  where
    readFrom :: FilePath -> IO (Either IOException [String])
    readFrom p = try $ do
      contents <- readFile p
      return $ lines contents
getSource (Options { inputFrom = StdIn }) = either throwError return =<< liftIO readStdIn
  where
    readStdIn :: IO (Either IOException [String])
    readStdIn = try $ do
      contents <- getContents
      return $ lines contents

getInput :: Options -> ExceptT IOException IO (Problem, [String])
getInput opts = do
  let p = problem opts
  input <- getSource opts
  return (p, input)
