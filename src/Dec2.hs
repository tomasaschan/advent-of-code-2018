module Dec2 where

  import Data.List
  import qualified Data.Map.Lazy as Map

  import Debug.Trace

  frequency :: Ord a => [a] -> [(a, Int)]
  frequency xs = Map.toList (Map.fromListWith (+) [ (x,1) | x <- xs ])

  hasMultiple :: Int -> String -> Bool
  hasMultiple n s = x
    where
      freqs = frequency s
      count = filter (\(_, n') -> n' == n) freqs
      x = length count > 0

  countId :: String -> (Int, Int)
  countId id' = (two, three) where
    two = if hasMultiple 2 id' then 1 else 0
    three = if hasMultiple 3 id' then 1 else 0

  tuplePlus :: (Int, Int) -> (Int, Int) -> (Int, Int)
  (a, b) `tuplePlus` (c, d) = (a + c, b + d)

  checksum :: [String] -> Int
  checksum ids = twos * threes
    where
      individual = map countId ids
      (twos, threes) = foldl tuplePlus (0,0) individual

  solveA :: [String] -> String
  solveA = show . checksum

  adjacent :: (String, String) -> Bool
  adjacent (a,b)
    | length a == length b = isAdjacent
    | otherwise = False
      where
        zipped = zip a b
        matched = map (\(a,b) -> a == b) zipped
        unmatched = filter not matched
        isAdjacent = length unmatched == 1

  findAdjacent :: [String] -> Maybe (String, String)
  findAdjacent [] = Nothing
  findAdjacent (a:[b])
    | adjacent (a,b) = Just (a,b)
    | otherwise = Nothing
  findAdjacent (a:b:rest)
    | adjacent (a,b) = Just (a,b)
    | otherwise = findAdjacent (b:rest)

  commonLetters :: Maybe (String, String) -> String
  commonLetters (Just (a,b)) = word
    where
      matching = map ((\x -> [x]) . fst) $ filter (\(a,b) -> a == b) $ zip a b
      word = intercalate "" matching

  solveB :: [String] -> String
  solveB = commonLetters . findAdjacent . sort