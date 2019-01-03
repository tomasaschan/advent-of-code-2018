module Simulation where

  import Data.List (elemIndex)

  simulate :: (a -> a) -> Int -> a -> [a]
  simulate next iterations state = _simulate [] iterations state
    where
      _simulate h 0 s = s:h
      _simulate h n s = _simulate h' n' s'
        where
          n' = n - 1
          s' = next s
          h' = s:h

  simulateToSteadyState :: Eq b => (a -> b) -> (Int -> a -> a) -> (a -> a) -> b -> Int -> a -> a
  simulateToSteadyState _ _ _ _ 0 s = s
  simulateToSteadyState store finisher next prev n s
    | prev == store s = finisher n s
    | otherwise = simulateToSteadyState store finisher next (store s) (n-1) (next s)

  simulateWithCycleDetection :: Eq b => (a -> b) -> (Int -> a -> a) -> (a -> a) -> Int -> a -> a
  simulateWithCycleDetection _ _ _ 0 state = state
  simulateWithCycleDetection store forwarder next iterations state = _simulateWithCycleDetection [] [] iterations state
    where
      _simulateWithCycleDetection h c n s = _simulateWithCycleDetection h' c' n' s'
        where
          (n', s', c', h') = case elemIndex (store s) c of
            Just i -> (n `mod` (i + 1), forwarder (n `div` (i + 1)) s, [], h)
            Nothing -> (n - 1, next s, store s : c, s:h)

  noop :: Int -> a -> a
  noop _ s = s
