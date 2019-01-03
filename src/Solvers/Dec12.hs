module Solvers.Dec12 where

  import Utils.List (minmax)
  import qualified Parse

  import Prelude hiding (lookup)
  import Data.List (iterate')
  import Data.Map.Strict (Map, toList, fromList, keys, lookup)
  import Data.Maybe (fromMaybe)

  type State = Map Int Char
  type Rules = Map String Char

  solveA :: [String] -> String
  solveA = solve 20
  solveB :: [String] -> String
  solveB = solve 50000000000

  solve :: Int -> [String] -> String
  solve generations input = show . sumPots . head . drop generations . iterate' stepOnce $ initial
    where
      initial = parseInitial . drop (length "initial state: ") . head $ input
      rules = parseRules . drop 2 $ input
      sumPots = sum . fmap fst . filter ((==) '#' . snd) . toList

      lookupWithDefault m k = fromMaybe '.' $ lookup k m

      stepOnce state = state' `seq` state'
        where
          (lo,hi) = minmax $ keys state
          rng = [lo-2..hi+2]
          state' = fromList . zip rng . fmap spread $ rng
            where
              spread :: Int -> Char
              spread i = lookupWithDefault rules surroundings
                where surroundings = fmap (lookupWithDefault state) [i-2..i+2]

  parseInitial :: String -> State
  parseInitial = fromList . zip [0..]

  parseRules :: [String] -> Rules
  parseRules = fromList . fmap parseRule
    where
      parseRule :: String -> (String, Char)
      parseRule input = extracted
        where
          parsed = Parse.substr 5 input
            >>= Parse.expect " => "
            >>= Parse.char
          extracted = Parse.extract _tuplify parsed
            where
              _tuplify [x,y] = (x, head y)
              _tuplify o = error ("unexpected parser output: " ++ show o)
