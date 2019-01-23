module Solvers.Dec16 where

  import Data.List.Split (splitOn)
  import Data.Maybe (catMaybes)

  import Text.ParserCombinators.Parsec

  import Data.Computer
  import Debug.Trace

  solveA :: [String] -> String
  solveA = show . length . filter (>=3) . fmap matchCount . parseA

  parseA :: [String] -> [Sample]
  parseA = catMaybes . fmap (parseMaybe sample) . splitOn "\n\n" . head . splitOn "\n\n\n" . unlines

  matchCount :: Sample -> Int
  matchCount = length . flip filter [minBound..maxBound] . flip matches

  matches :: Op -> Sample -> Bool
  matches op s = actual == expected
    where
      expected  = after s
      (_,a,b,c) = operation s
      actual    = apply op a b c $ before s

  data Sample = Sample {
    before :: Registry,
    operation :: (Int,Int,Int,Int),
    after :: Registry
  } deriving (Eq, Show)

  parseMaybe :: GenParser Char () a -> String -> Maybe a
  parseMaybe parser input = do
    case parse parser "input" input of
      Left e -> trace (show e) Nothing
      Right s -> Just s

  sample :: GenParser Char st Sample
  sample = do
    b <- string "Before:" *> whitespace *> registry <* newline
    o <- maskedOperation <* newline
    a <- string "After:" *> whitespace *> registry
    return Sample { before = b, operation = o, after = a }

  registry :: GenParser Char st Registry
  registry = fmap fromList $ char '[' *> number `sepBy1` (string ", ") <* char ']'

  maskedOperation :: GenParser Char st (Int,Int,Int,Int)
  maskedOperation = do
    is <- number `sepBy1` (string " ")
    case is of
      [a,b,c,d] -> return (a,b,c,d)
      _ -> fail "did not match list of four numbers"

  number :: GenParser Char st Int
  number = fmap read $ many1 digit
  whitespace :: GenParser Char st String
  whitespace = many (oneOf " \t")
