module Solver where

import Data.Map

import Cave
import Explore

parse :: [String] -> (Depth, Target)
parse = interpret . fmap (last . words)
    where
        interpret [d, t] = (Depth $ read d, Target $ read ("(" ++ t ++ ")"))
        interpret _      = undefined

totalRisk :: ErosionMap -> Int
totalRisk (ErosionMap _ _ m) = sum $ riskLevel . assess <$> elems m

mapOutCave :: Depth -> Target -> ErosionMap
mapOutCave depth (Target target) = explore target $ emptyErosion depth (Target target)



a :: [String] -> String
a =  show . totalRisk . uncurry mapOutCave . parse

b :: [String] -> String
b = show . uncurry findRudolph . parse
