module Main where

import Control.Monad.Except
import Control.Exception (IOException)
import Control.Concurrent (threadDelay)
import Text.Printf

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
  threadDelay $ 50 * 1000

pauseAfter :: (a -> IO ()) -> (a -> IO ())
pauseAfter f = \w -> do
  f w
  pause

runProblem :: (Problem, [String]) -> ExceptT IOException IO ()
runProblem (problem, input) = do
  let (World dungeon units) = initial problem input
  liftIO $ do
    let (history, turns) = play dungeon units
    sequence_ . fmap (pauseAfter (uncurry $ flip showState dungeon)) . zip [0..] $ history
    endCombat dungeon

    let (_, totalHP, outcome) = solveA (history, turns)
    putStrLn $ printf "Solution to A: %d (%d hp after %d finished rounds)" outcome totalHP turns

renderError :: Show e => e -> IO ()
renderError e = putStrLn $ "Error: " ++ show e
