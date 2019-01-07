module Utils.List where

  import Data.Foldable (foldl')
  import qualified Data.Set as Set

  (!?) :: [a] -> Int -> Maybe a
  xs !? i
    | 0 <= i && i < length xs = Just (xs !! i)
    | otherwise = Nothing

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
