module Utils where

  import System.IO
  import System.CPUTime
  import System.Directory
  import Text.Printf

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
    let solver = solve (solvers problem)
    return $ solver input
  liftSolver _ Nothing = do
    return "Usage: aoc <problem> <input-file>"
