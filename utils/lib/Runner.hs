module Runner where

import           Text.Printf

readInput :: IO [String]
readInput = do
  contents <- getContents
  return . lines $ contents

printPartialSolution :: String -> String -> IO ()
printPartialSolution label answer = do
  putStrLn $ printf "%s: %s" label answer

run1 :: String -> ([String] -> String) -> IO ()
run1 label solver = do
  input <- readInput
  let answer = solver input
  printPartialSolution label answer

run2 :: ([String] -> String) -> ([String] -> String) -> IO ()
run2 a b = do
  input <- readInput
  let a' = a input
  let b' = b input
  printPartialSolution "a" a'
  printPartialSolution "b" b'
