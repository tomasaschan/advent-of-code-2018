module Solver where

import           Data.Graph
import           Data.List
import           Data.Text                      ( pack
                                                , splitOn
                                                , unpack
                                                )

data Point = Point Int Int Int Int
    deriving (Show, Eq)

parse :: String -> Point
parse = pontify . fmap (read . unpack) . splitOn (pack ",") . pack
  where
    pontify [x, y, z, u] = Point x y z u
    pontify wat = errorWithoutStackTrace $ "Invalid point data: " ++ show wat

pairs :: [a] -> [(a, a)]
pairs l = [ (x, y) | (x : ys) <- tails l, y <- ys ]

distance :: Point -> Point -> Int
distance (Point x y z u) (Point x' y' z' u') =
    abs (x - x') + abs (y - y') + abs (z - z') + abs (u - u')

buildGraph :: [Point] -> Graph
buildGraph points =
    let edges' =
            fmap (\((i, _), (j, _)) -> (i, j))
                . filter (\((_, u), (_, v)) -> distance u v <= 3)
                . pairs
                . zip ([1 ..] :: [Int])
                $ points
    in  buildG (1, length points) edges'

a :: [String] -> String
a = show . length . components . buildGraph . fmap parse
