module Data.Foldable.Extended
  ( minmax
  , minmaxBy
  , maximums
  , mapSize
  , module Data.Foldable
  )
where

import           Data.Foldable

mapSize :: (Foldable t, Ord a, Bounded a) => t (a, a) -> ((a, a), (a, a))
mapSize = foldl' folder ((maxBound, maxBound), (minBound, minBound))
 where
  folder ((xlo, ylo), (xhi, yhi)) (x, y) =
    ((min xlo x, min ylo y), (max xhi x, max yhi y))

minmaxBy :: (Foldable t, Ord b, Bounded b) => (a -> b) -> t a -> (b, b)
minmaxBy f = foldl' folder (maxBound, minBound)
  where folder (lo, hi) a = (min lo (f a), max hi (f a))

minmax :: (Foldable t, Ord a, Bounded a) => t a -> (a, a)
minmax = minmaxBy id

maximums :: (Foldable t, Ord a, Bounded a) => t (a, a) -> (a, a)
maximums = foldl' folder (minBound, minBound)
  where folder (xhi, yhi) (x, y) = (max xhi x, max yhi y)
