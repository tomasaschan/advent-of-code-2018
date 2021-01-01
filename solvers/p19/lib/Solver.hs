module Solver where

import Computer

import qualified Parse

a :: [String] -> String
a = either show (show . sum . factorize . findTargetNumber 0) . Parse.program

b :: [String] -> String
b = either show (show . sum . factorize . findTargetNumber 1) . Parse.program

findTargetNumber :: Int -> (Register, [Instruction]) -> Int
findTargetNumber r0 input =
   let
      (p, s) = initialize r0 input
      done _ (State _ ip _) = ip == 2
      s' = runUntil done p s
   in get (Register Five) s'

factorize :: Int -> [Int]
factorize n =
    let factorize' :: Int -> [Int] -> [Int]
        factorize' 0 fs                  = fs
        factorize' i fs | n `mod` i == 0 = factorize' (i-1) (i:fs)
        factorize' i fs                  = factorize' (i-1) fs
     in factorize' n []
