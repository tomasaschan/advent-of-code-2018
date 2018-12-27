module Aoc2017.Dec1 (
  sumString,
  Shift (..),
  solveA,
  solveB
) where

import Text.Printf

shift :: Int -> [a] -> [a]
shift s lst = back ++ front
  where
    (front,back) = splitAt s lst

pairs :: Int -> String -> [(String, String)]
pairs s lst = strings
  where
    chars = zip lst (shift s lst)
    stringify c = [c]
    strings = map (\(a,b) -> (stringify a, stringify b)) chars

pairIsValid :: (String, String) -> Bool
pairIsValid (a,b) = a == b

data Shift = One | Half deriving (Show, Eq)

sumString :: Shift -> String -> Int
sumString s input = sum numbers
  where
    n = case s of
      One -> 1
      Half -> div (length input) 2
    ps = pairs n input
    validPairs = filter pairIsValid ps
    numbers = map (read . fst) validPairs :: [Int]

solve :: Shift -> [String] -> String
solve s = printf "%d" . sumString s . head

solveA :: [String] -> String
solveA = solve One
solveB :: [String] -> String
solveB = solve Half
