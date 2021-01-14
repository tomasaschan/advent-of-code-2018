module Solver where

import Debug.Trace
import Data.Map
import Data.Default.Class

import Cave

parse :: [String] -> (Int, Coord)
parse = interpret . fmap (last . words)
    where
        interpret [d, t] = (read d, read ("(" ++ t ++ ")"))
        interpret _      = undefined

mapOutCave :: Int -> Coord -> Cave
mapOutCave depth target = assess . explore depth target target $ def

a :: [String] -> String
a =  show . sum . fmap riskLevel . (\(Cave m) -> elems m) . uncurry mapOutCave . parse
