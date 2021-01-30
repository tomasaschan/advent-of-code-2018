module Solver where

import           Control.Applicative           (liftA2)
import           Data.Computer
import           Data.Either.Extended
import           Data.IntMap                   (IntMap)
import qualified Data.IntMap                   as IntMap
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Text.ParserCombinators.Parsec

data Sample =
  Sample
    { before :: Registry
    , opCode :: Int
    , args   :: (Int, Int, Int)
    , after  :: Registry
    }
  deriving (Eq, Show)

solveA :: [String] -> String
solveA =
  show .
  fmap (length . filter (>= 3) . fmap matchCount . fst) .
  parse parseInput "input" . unlines

solveB :: [String] -> String
solveB input =
  either show show $ do
    parsed <- parse16 input
    let opCodes =
          refine IntMap.empty . foldl (flip learn) anythingsPossible . fst $
          parsed
    instructions <- foldAll . fmap (interpret opCodes) . snd $ parsed
    return . valueAt 0 . foldl (flip run) (initial 4) $ instructions
  where
    run :: (Op, Int, Int, Int) -> Registry -> Registry
    run (o, x, y, z) r = apply o x y z r

interpret ::
     IntMap Op -> (Int, Int, Int, Int) -> Either String (Op, Int, Int, Int)
interpret codes (o, x, y, z) =
  case IntMap.lookup o codes of
    Just op -> Right (op, x, y, z)
    Nothing ->
      Left $
      "the interpretation of " ++
      show o ++ " was not determined by the learning steps"

allOps :: Set Op
allOps = Set.fromList [minBound .. maxBound]

anythingsPossible :: IntMap (Set Op)
anythingsPossible = IntMap.fromList $ fmap (\i -> (i, allOps)) [0 .. 15]

excludedPossibilities :: Sample -> Set Op
excludedPossibilities s = Set.filter (not . matches s) allOps

learn :: Sample -> IntMap (Set Op) -> IntMap (Set Op)
learn s =
  IntMap.adjust (\k -> k `Set.difference` (excludedPossibilities s)) (opCode s)

refine :: IntMap Op -> IntMap (Set Op) -> IntMap Op
refine known partials
  | all null partials = known
  | otherwise =
    let determined =
          IntMap.map (Set.elemAt 0) . IntMap.filter ((== 1) . Set.size) $
          partials
        known' = known `IntMap.union` determined
        partials' =
          IntMap.map
            (flip Set.difference $ Set.fromList . IntMap.elems $ known')
            partials
     in refine known' partials'

matchCount :: Sample -> Int
matchCount = length . flip filter [minBound .. maxBound] . matches

matches :: Sample -> Op -> Bool
matches s op = actual == expected
  where
    expected = after s
    (a, b, c) = args s
    actual = apply op a b c $ before s

parse16 :: [String] -> Either String ([Sample], [(Int, Int, Int, Int)])
parse16 = showErr . parse parseInput "input" . unlines
  where
    showErr :: Show e => Either e a -> Either String a
    showErr (Left e)  = Left $ show e
    showErr (Right x) = Right x

parseInput :: GenParser Char () ([Sample], [(Int, Int, Int, Int)])
parseInput = do
  samples <- sample `sepTry` (count 2 newline) <* count 4 newline
  operations <- maskedOperation `sepTry` newline <* newline <* eof
  return (samples, operations)
      -- see https://stackoverflow.com/a/54336966/38055
  where
    sepTry a sep = sepTry1 a sep <|> pure []
    sepTry1 a sep = liftA2 (:) a (many (try $ sep *> a))

sample :: GenParser Char st Sample
sample = do
  b <- string "Before:" *> spaces *> registry <* newline
  (o, x, y, z) <- maskedOperation <* newline
  a <- string "After:" *> spaces *> registry
  return Sample {before = b, opCode = o, args = (x, y, z), after = a}

registry :: GenParser Char st Registry
registry = fmap fromList $ char '[' *> number `sepBy1` (string ", ") <* char ']'

maskedOperation :: GenParser Char st (Int, Int, Int, Int)
maskedOperation = do
  is <- number `sepBy1` (string " ")
  case is of
    [a, b, c, d] -> return (a, b, c, d)
    _            -> fail "did not match list of four numbers"

number :: GenParser Char st Int
number = fmap read $ many1 digit
