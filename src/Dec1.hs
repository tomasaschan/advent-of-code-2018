module Dec1 (
  solveA,
  solveB,
  addP
)
where

  import Data.Set (Set, empty, insert, member, singleton)

  parseFreq :: String -> Int
  parseFreq ('+':s) = read s
  parseFreq s = read s

  solveA :: [String] -> String
  solveA = show . sum . (map parseFreq)

  addP :: Int -> Set Int -> [Int] -> Int
  addP current seen (this:rest)
    | member (current + this) seen = current + this
    | otherwise =
      addP next (insert next seen) rest where
        next = current + this

  solveB :: [String] -> String
  solveB = show . addP 0 (singleton 0) . cycle . map parseFreq
