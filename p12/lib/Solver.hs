module Solver
  ( solveA
  , solveB
  , parseInitial
  , parseRules
  ) where

import           Data.Foldable.Extended (minmax)
import qualified Parse

import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map (fromList, lookup)
import           Data.Maybe             (fromMaybe)
import           Data.Set               (Set)
import qualified Data.Set               as Set (fromList, toList)
import           Prelude                hiding (lookup)

import           Simulation

type State = Set Int

type Rules = Map String Char

solveA :: [String] -> String
solveA = solve 20

solveB :: [String] -> String
solveB = solve 50000000000

solve :: Int -> [String] -> String
solve generations input = show . sum . run $ initial
  where
    initial = parseInitial . head $ input
    rules = parseRules . drop 2 $ input
    run = simulateToSteadyState store finish stepOnce "" generations
    finish n = Set.fromList . fmap (+ n) . Set.toList
    lookupWithDefault m k = fromMaybe '.' $ Map.lookup k m
    stepOnce :: State -> State
    stepOnce state = state'
      where
        (lo, hi) = minmax state
        state' = interpretString (lo - 2) $ fmap spread [lo - 2 .. hi + 2]
          where
            spread :: Int -> Char
            spread i = lookupWithDefault rules surroundings
              where
                surroundings = asString (i - 2) (i + 2) state

store :: State -> String
store s = asString lo hi s
  where
    (lo, hi) = minmax s

parseInitial :: String -> State
parseInitial = interpretString 0 . drop (length "initial state: ")

interpretString :: Int -> String -> State
interpretString offset =
  Set.fromList . fmap fst . filter ((== '#') . snd) . zip [offset ..]

asString :: Int -> Int -> State -> String
asString lo hi state = fmap (toC state) [lo .. hi]
  where
    toC s i' =
      if i' `elem` s
        then '#'
        else '.'

parseRules :: [String] -> Rules
parseRules = Map.fromList . fmap parseRule
  where
    parseRule :: String -> (String, Char)
    parseRule input = extracted
      where
        parsed = Parse.substr 5 input >>= Parse.expect " => " >>= Parse.char
        extracted = Parse.extract _tuplify parsed
          where
            _tuplify [x, y] = (x, head y)
            _tuplify o      = error ("unexpected parser output: " ++ show o)
