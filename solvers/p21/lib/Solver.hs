module Solver where

import Data.Bifunctor
import Data.Set (Set)

import Computer
import Computer.Parse

data S = S Register Int Memory (Set Int) deriving (Show, Eq)

patch :: (Int -> Register -> Instruction -> Step S) -> (Register, [Instruction]) -> (Register, [Step S])
patch patcher (r, is) = (r, fmap (patcher (length is) r) is)

state :: (Register, [Step S]) -> (Program S, S)
state (r, ss) =
    let p = Program ss
        s = S r 0 zeroed mempty
    in (p, s)

instance State S where
    register (S r _ _ _) = r
    ip       (S _ p _ _) = p
    memory   (S _ _ m _) = m

    withIp     p (S r _ m s) = S r p m s
    withMemory m (S r p _ s) = S r p m s

isAtEq :: Program S -> S -> Bool
isAtEq p = isEq . current p
    where isEq (Op (Eq (Register _) (Register _) _)) = True
          isEq _                                     = False

solve :: ((Register, [Instruction]) -> Int) -> [String] -> String
solve solver = either show (show . solver) . program

a :: (Register, [Instruction]) -> Int
a = get (Register Five) . uncurry (runUntil isAtEq) . state . second (fmap Op)
