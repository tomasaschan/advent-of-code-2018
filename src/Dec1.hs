module Dec1 (
  solveA,
  solveB,
  addP
)
where

  parseFreq :: String -> Int
  parseFreq ('+':s) = read s
  parseFreq s = read s

  solveA :: [String] -> String
  solveA = show . sum . (map parseFreq)

  addP :: Int -> [Int] -> [Int] -> Int
  addP current seen (this:rest)
    | current + this `elem` seen = current + this
    | otherwise =
      addP next (next:seen) rest where
        next = current + this

  solveB :: [String] -> String
  solveB = show . addP 0 [0] . cycle . map parseFreq
