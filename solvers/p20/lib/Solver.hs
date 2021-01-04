module Solver where

import Data.List (intercalate)

import Parse
import Domain

solve1 :: String -> String
solve1 = either show (show . maximum . fmap lengthWithoutLoops . followAll []) . path

a :: [String] -> String
a = intercalate "\n" . fmap solve1
