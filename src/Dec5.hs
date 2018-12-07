module Dec5 where

  import Data.Char
  import Data.List

  solveA :: [String] -> String
  solveA = show . length . annihilate . head

  solveB :: [String] -> String
  solveB [input] =
    let
      input' = annihilate input
      getLengths = map (\c -> improvedLength c) (units input')
      lenghts = map (\f -> f input') getLengths
      best = minimum lenghts
    in show best
  solveB _ = "invalid input format"

  improvedLength :: Char -> (String -> Int)
  improvedLength c = length . annihilate . remove c

  units :: String -> String
  units = nub . map toLower

  remove :: Char -> String -> String
  remove c = filter (\x -> toLower x /= toLower c)

  annihilate :: String -> String
  annihilate s
    | passOnce "" s == reverse s = s
    | otherwise = annihilate $ reverse $ passOnce "" s

  annihilates :: Char -> Char -> Bool
  annihilates a b = a /= b && toLower a == toLower b

  passOnce :: String -> String -> String
  passOnce processed (a:b:rest)
    | annihilates a b = passOnce processed rest
    | otherwise = passOnce (a:processed) (b:rest)
  passOnce processed [a] = a:processed
  passOnce processed "" = processed
