module Input where

import CLI

import Control.Exception (try, IOException)
import Control.Monad.Except

data Part = PartA | PartB | Both deriving (Show, Eq)

problem :: Options -> Part
problem (Options { solvePart1 = True, solvePart2 = False }) = PartA
problem (Options { solvePart1 = False, solvePart2 = True }) = PartB
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

getInput :: Options -> ExceptT IOException IO (Part, [String])
getInput opts = do
  let p = problem opts
  input <- getSource opts
  return (p, input)
