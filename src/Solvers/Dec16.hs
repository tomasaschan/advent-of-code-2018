module Solvers.Dec16 where

  import Data.List.Split (splitOn)
  import Data.Maybe (catMaybes)

  import Text.ParserCombinators.Parsec

  import Data.Computer

  solveA :: [String] -> String
  solveA = show . parseA

  parseA :: [String] -> [Sample]
  parseA = catMaybes . fmap (parseMaybe sample) . splitOn "\n\n" . head . splitOn "\n\n\n" . unlines

  data Sample = Sample {
    before :: Registry,
    operation :: (Int,Int,Int,Int),
    after :: Registry
  } deriving (Eq, Show)

  parseMaybe :: GenParser Char () a -> String -> Maybe a
  parseMaybe parser input = do
    case parse parser "input" input of
      Left e -> Nothing
      Right s -> Just s

  sample :: GenParser Char st Sample
  sample = do
    before <- string "Before:" *> whitespace *> registry <* newline
    operation <- maskedOperation <* newline
    after <- string "After:" *> whitespace *> registry
    return Sample { before = before, after = after, operation = operation }

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
