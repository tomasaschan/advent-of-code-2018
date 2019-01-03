module Utils.List where

  import Data.Foldable (foldl')

  (!?) :: [a] -> Int -> Maybe a
  xs !? i
    | 0 <= i && i < length xs = Just (xs !! i)
    | otherwise = Nothing

  minmax :: (Foldable t, Ord a, Bounded a) => t a -> (a, a)
  minmax = foldl' folder (maxBound, minBound)
    where
      folder (lo,hi) a = (min lo a, max lo a)
