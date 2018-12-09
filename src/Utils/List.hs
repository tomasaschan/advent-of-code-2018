module Utils.List where

  (!?) :: [a] -> Int -> Maybe a
  xs !? i
    | 0 <= i && i < length xs = Just (xs !! i)
    | otherwise = Nothing
