-- {-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
module Solver where

import qualified Data.Set                      as Set
import qualified Parse

import           Debug.Trace

debug :: Show a => String -> a -> a
debug lbl x = trace (lbl ++ ": " ++ show x) x

type Star = ((Int, Int), (Int, Int))

getpos :: Star -> (Int, Int)
getpos = fst

getv :: Star -> (Int, Int)
getv = snd

getx :: (Int, Int) -> Int
getx = fst

gety :: (Int, Int) -> Int
gety = snd

pretty :: [Star] -> String
pretty sky = unlines
  [ [ marker x y | x <- [xmin .. xmax] ] | y <- [ymin .. ymax] ]
 where
  positions = Set.fromList . fmap getpos $ sky
  (xmin, xmax, ymin, ymax) =
    foldl folder (maxBound, minBound, maxBound, minBound) . fmap getpos $ sky
   where
    folder (xlo, xhi, ylo, yhi) (x, y) =
      (min xlo x, max xhi x, min ylo y, max yhi y)
  marker x y = if (x, y) `Set.member` positions then '#' else '.'

simulate :: (Int, [Star]) -> (Int, [Star])
simulate (t, sky) | height next < height sky = simulate (t + 1, next)
                  | otherwise                = (t, sky)
 where
  next = step sky
  step :: [Star] -> [Star]
  step sky' = fmap move sky'
    where move ((x, y), (vx, vy)) = ((x + vx, y + vy), (vx, vy))
  height sky' = ymax - ymin
   where
    (ymin, ymax) = foldl folder (0, 0) . fmap gety . fmap getpos $ sky'
      where folder (mn, mx) y = (min mn y, max mx y)

solve :: [String] -> String
solve = output . simulate . ((,) 0) . fmap parse
  where output (t, sky) = show t ++ "\n" ++ pretty sky

parse :: String -> Star
parse input = Parse.extract (particle . fmap read) parsed
 where
  particle :: [Int] -> Star
  particle [x, y, vx, vy] = ((x, y), (vx, vy))
  particle x              = error . (++) "invalid parse output: " . show $ x
  parsed :: Parse.Parser String
  parsed =
    Parse.expect "position=<" input
      >>= Parse.number
      >>= Parse.expect ", "
      >>= Parse.number
      >>= Parse.expect "> velocity=<"
      >>= Parse.number
      >>= Parse.expect ", "
      >>= Parse.number
      >>= Parse.expect ">"
