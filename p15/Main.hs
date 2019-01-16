module Main where

import Control.Monad.Except
import Control.Exception (IOException)
import Control.Concurrent (threadDelay)

import CLI
import Input
import Output

import Solvers.Dec15

main :: IO ()
main = either renderError return =<< runExceptT runMain

runMain :: ExceptT IOException IO ()
runMain = do
  opts <- liftIO $ getOptions
  p <- getInput opts
  runProblem p

pause :: IO ()
pause = do
  threadDelay 100

pauseAfter :: IO () -> IO ()
pauseAfter f = do
  f
  pause

runProblem :: (Problem, [String]) -> ExceptT IOException IO ()
runProblem (problem, input) = do
  let s = initial problem input
  liftIO $ do
    sequence_ . fmap pauseAfter . fmap showState . play $ s
    endProgram s

renderError :: Show e => e -> IO ()
renderError e = putStrLn $ "Error: " ++ show e
