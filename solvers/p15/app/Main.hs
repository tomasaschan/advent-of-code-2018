module Main where

import           Control.Concurrent   (threadDelay)
import           Control.Exception    (IOException)
import           Control.Monad.Except
import           Text.Printf

import           CLI
import           Input
import           Output

import           Solver

main :: IO ()
main = either renderError return =<< runExceptT runMain

runMain :: ExceptT IOException IO ()
runMain = do
  opts <- liftIO $ getOptions
  input <- getSource $ opts
  case problem opts of
    PartA -> runA input
    PartB -> runB input
    Both -> do
      runA input
      runB input

pause :: IO ()
pause = do
  threadDelay $ 50 * 1000

pauseAfter :: (a -> IO ()) -> (a -> IO ())
pauseAfter f =
  \w -> do
    f w
    pause

runA :: [String] -> ExceptT IOException IO ()
runA input = do
  let (dungeon, units) = initial 3 input
  liftIO $ do
    let (history, turns) = play dungeon units
    sequence_ .
      fmap (pauseAfter (uncurry $ flip showState dungeon)) . zip [0 ..] $
      history
    endCombat dungeon
    let (_, totalHP, oc, _) = summarize (history, turns)
    putStrLn $
      printf
        "Solution to A: %d (%d hp after %d finished rounds)"
        oc
        totalHP
        turns

runB :: [String] -> ExceptT IOException IO ()
runB input =
  liftIO $ do
    let answer = solveB input
    putStrLn $ printf "Solution to B: %s" answer

renderError :: Show e => e -> IO ()
renderError e = putStrLn $ "Error: " ++ show e
