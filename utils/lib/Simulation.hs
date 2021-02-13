module Simulation where

import           Data.List                      ( elemIndex )

simulate :: (a -> a) -> Int -> a -> [a]
simulate next iterations state = _simulate [] iterations state
 where
  _simulate h 0 s = s : h
  _simulate h n s = _simulate h' n' s'
   where
    n' = n - 1
    s' = next s
    h' = s : h

simulateUntil :: Int -> (a -> Bool) -> (a -> a) -> a -> [a]
simulateUntil maxIterations isDone next initial = _simulateUntil 0 [] initial
 where
  _simulateUntil n h s | n == maxIterations || isDone s = s : h
                       | otherwise = _simulateUntil (n + 1) (s : h) (next s)

simulateToSteadyState
  :: Eq b => (a -> b) -> (Int -> a -> a) -> (a -> a) -> b -> Int -> a -> a
simulateToSteadyState _ _ _ _ 0 s = s
simulateToSteadyState store finisher next prev n s
  | prev == store s = finisher n s
  | otherwise = simulateToSteadyState store
                                      finisher
                                      next
                                      (store s)
                                      (n - 1)
                                      (next s)

simulateWithCycleDetection
  :: Eq b => (a -> b) -> (Int -> a -> a) -> (a -> a) -> Int -> a -> a
simulateWithCycleDetection _ _ _ 0 state = state
simulateWithCycleDetection store forwarder next iterations state =
  _simulateWithCycleDetection [] [] iterations state
 where
  _simulateWithCycleDetection _ _ 0 s = s
  _simulateWithCycleDetection h c n s = _simulateWithCycleDetection h' c' n' s'
   where
    (n', s', c', h') = case elemIndex (store s) c of
      Just i  -> (n `mod` (i + 1), forwarder (n `div` (i + 1)) s, [], h)
      Nothing -> (n - 1, next s, store s : c, s : h)

noop :: Int -> a -> a
noop _ s = s
