module Solver where

import           ImmuneSystem
import qualified Parse

a :: [String] -> String
a = either show (show . fst . fight) . Parse.armies . unlines

b :: [String] -> String
b = either show (show . findSmallestBoost) . Parse.armies . unlines
