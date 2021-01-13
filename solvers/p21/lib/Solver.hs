module Solver where

import Data.Bifunctor
import Data.Maybe
import Data.Set (Set, insert)

import Computer
import Computer.Parse

data S = S Register Int Memory (Set Int) (Maybe Int) deriving (Show, Eq)

lastF :: S -> Maybe Int
lastF (S _ _ _ _ i) = i

seen :: S -> Set Int
seen (S _ _ _ s _ ) = s

see :: Int -> S -> S
see i (S r p m s _) = S r p m (insert i s) (Just i)

state :: (Register, [Step S]) -> (Program S, S)
state (r, ss) =
    let p = Program ss
        s = S r 0 zeroed mempty Nothing
    in (p, s)

instance State S where
    register (S r _ _ _ _) = r
    ip       (S _ p _ _ _) = p
    memory   (S _ _ m _ _) = m

    withIp     p (S r _ m s i) = S r p m s i
    withMemory m (S r p _ s i) = S r p m s i

isAtEq :: Program S -> S -> Bool
isAtEq p = isEq . current p
    where isEq (Op (Eq (Register _) (Register _) _)) = True
          isEq _                                     = False

solve :: ((Register, [Instruction]) -> Int) -> [String] -> String
solve solver = either show (show . solver) . program

a :: (Register, [Instruction]) -> Int
a = get (Register Five) . uncurry (runUntil isAtEq) . state . second (fmap Op)

patch :: Instruction -> Step S
patch (Eq (Register Five) (Register Zero) Three) = Custom f
    where f s = let i  = get (Register Five) s
                    f' = if i `notElem` seen s
                         then see i . flip apply (Op (Eq (Register Five) (Register Zero) Three))
                         else withIp (-10) -- terminate
                in f' s
patch i                                          = Op i

b :: (Register, [Instruction]) -> Int
b = fromMaybe 0 . lastF . uncurry (runUntil terminated) . state . second (fmap patch)
