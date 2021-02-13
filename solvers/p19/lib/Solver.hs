module Solver where

import           Computer

import           Computer.Parse

a :: [String] -> String
a = either show (show . sum . factorize . findTargetNumber 0) . program

b :: [String] -> String
b = either show (show . sum . factorize . findTargetNumber 1) . program

data S = S Register Int Memory deriving (Show)
instance State S where
  register (S r _ _) = r
  ip (S _ p _) = p
  memory (S _ _ m) = m

  withMemory m (S r p _) = S r p m
  withIp p (S r _ m) = S r p m

findTargetNumber :: Int -> (Register, [Instruction]) -> Int
findTargetNumber r0 input =
  let initialize (ptr, is) =
          (Program $ Op <$> is, S ptr 0 $ set' Zero (Immediate r0) zeroed)
      (p, s) = initialize input
      s'     = runUntil (const ((== 2) . ip)) p s
  in  get (Register Five) s'

factorize :: Int -> [Int]
factorize n =
  let factorize' :: Int -> [Int] -> [Int]
      factorize' 0 fs                  = fs
      factorize' i fs | n `mod` i == 0 = factorize' (i - 1) (i : fs)
      factorize' i fs                  = factorize' (i - 1) fs
  in  factorize' n []
