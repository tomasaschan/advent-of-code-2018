module Utils.List where

  import Data.Foldable (foldl')
  import qualified Data.Set as Set

  (!?) :: [a] -> Int -> Maybe a
  xs !? i
    | 0 <= i && i < length xs = Just (xs !! i)
    | otherwise = Nothing

  minmax :: (Foldable t, Ord a, Bounded a) => t a -> (a, a)
  minmax = foldl' folder (maxBound, minBound)
    where
      folder (lo,hi) a = (min lo a, max hi a)

  maximums :: (Foldable t, Ord a, Bounded a) => t (a,a) -> (a,a)
  maximums = foldl' folder (minBound, minBound)
    where
      folder (xhi, yhi) (x,y) = (max xhi x, max yhi y)

  readingOrder :: Ord a => (a,a) -> (a,a) -> Ordering
  readingOrder (x,y) (x',y')
    | cy /= EQ = cy
    | otherwise = cx
    where
      cx = compare x x'
      cy = compare y y'

  hasDuplicates :: Ord a => [a] -> Bool
  hasDuplicates = hasDuplicatesWith Set.empty
    where
      hasDuplicatesWith seen [] = False
      hasDuplicatesWith seen (x:xs) = x `elem` seen || hasDuplicatesWith (Set.insert x seen) xs
