module Utils.Foldable where

  import Data.Foldable (foldl')

  minmax :: (Foldable t, Ord a, Bounded a) => t a -> (a, a)
  minmax = foldl' folder (maxBound, minBound)
    where
      folder (lo,hi) a = (min lo a, max hi a)

  maximums :: (Foldable t, Ord a, Bounded a) => t (a,a) -> (a,a)
  maximums = foldl' folder (minBound, minBound)
    where
      folder (xhi, yhi) (x,y) = (max xhi x, max yhi y)
