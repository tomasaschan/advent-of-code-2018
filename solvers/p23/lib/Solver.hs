module Solver where

import           Data.Either
import           Data.List                      ( sortOn )
import           Data.Ord
import           Data.PQueue.Max         hiding ( filter )
import           Domain

import qualified Parse

isInRangeOf :: Nanobot -> Position -> Bool
isInRangeOf bot pos = manhattan (position bot) pos < range bot

a :: [String] -> String
a input =
    let
        bots                 = rights $ Parse.nanobot <$> input
        botWithLargestRadius = head . sortOn (Down . range) $ bots
        botsInRange = filter (isInRangeOf botWithLargestRadius . position) bots
        solution             = length botsInRange
    in
        show solution

b :: [String] -> String
b input =
    let bots = rights $ Parse.nanobot <$> input
        q0   = singleton $ initialCube bots
        search q = case deleteFindMax q of
            (sq, _) | side sq == 1 -> manhattan origo $ anchor sq
            (sq, q') ->
                let cubes = split sq
                    q''   = foldl (flip insert) q' cubes
                in  search q''
    in  show $ search q0
