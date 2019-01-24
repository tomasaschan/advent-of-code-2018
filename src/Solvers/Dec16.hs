module Solvers.Dec16 where

  import Data.Maybe (fromJust)
  import Text.ParserCombinators.Parsec
  import Control.Applicative (liftA2)

  import Data.Computer
  import Debug.Trace

  data Sample = Sample {
    before :: Registry,
    operation :: (Int,Int,Int,Int),
    after :: Registry
  } deriving (Eq, Show)

  solveA :: [String] -> String
  solveA = show . length . filter (>=3) . fmap matchCount . fst . fromJust . parseMaybe parseInput . unlines

  matchCount :: Sample -> Int
  matchCount = length . flip filter [minBound..maxBound] . flip matches

  matches :: Op -> Sample -> Bool
  matches op s = actual == expected
    where
      expected  = after s
      (_,a,b,c) = operation s
      actual    = apply op a b c $ before s

  parseMaybe :: GenParser Char () a -> String -> Maybe a
  parseMaybe parser input = do
    case parse parser "input" input of
      Left e -> trace (show e) Nothing
      Right s -> Just s

  parseInput :: GenParser Char () ([Sample], [(Int,Int,Int,Int)])
  parseInput = do
    samples <- sample `sepTry` (count 2 newline) <* count 4 newline
    operations <- maskedOperation `sepTry` newline <* newline <* eof
    return (samples, operations)
    where
      -- see https://stackoverflow.com/a/54336966/38055
      sepTry a sep = sepTry1 a sep <|> pure []
      sepTry1 a sep = liftA2 (:) a (many (try $ sep *> a))

  sample :: GenParser Char st Sample
  sample = do
    b <- string "Before:" *> spaces *> registry <* newline
    o <- maskedOperation <* newline
    a <- string "After:" *> spaces *> registry
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
