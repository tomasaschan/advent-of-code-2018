module Solver where

import qualified Data.Map as Map
import Parse2
import Domain

exploreBase :: String -> Explored
exploreBase = explore . createMap . path

distances :: Explored -> [Int]
distances (Explored _ m) = Map.elems m

a :: [String] -> String
a = show . maximum . distances . exploreBase . head . stripComments

b :: [String] -> String
b = show . length . filter (>=1000) . distances . exploreBase . head . stripComments

stripComments :: [String] -> [String]
stripComments = filter (not . isComment)
  where
    isComment ('#':_) = True
    isComment _       = False
