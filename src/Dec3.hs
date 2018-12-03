module Dec3 where

  import Text.Regex (Regex, mkRegex, matchRegex)
  import Data.Map (Map, alter, empty, toList, lookup, unionWith, elems)
  import qualified Data.List as List (foldr)
  import Data.Maybe (catMaybes)

  solveA :: [String] -> String
  solveA = show . countOverlaps . combine . map fabric . catMaybes . map parse

  data Claim = Claim {
    top :: Int,
    left :: Int,
    width :: Int,
    height :: Int
  } deriving (Show, Eq)

  type Square = (Int,Int)
  type Fabric = (Map Square Int)

  countOverlaps :: Fabric -> Int
  countOverlaps = length . filter (\i -> i > 1) . elems

  combine :: [Fabric] -> Fabric
  combine = List.foldr (unionWith (+)) fempty

  inc :: Maybe Int -> Maybe Int
  inc Nothing = Just 1
  inc (Just i) = Just (i+1)

  claim1 :: Fabric -> Square -> Fabric
  claim1 fabric square = alter inc square fabric

  squares :: Claim -> [Square]
  squares (Claim { top = top, left = left, width = width, height = height }) = [(x,y) | y <- ys, x <- xs]
    where
      xs = [left..(left+width-1)]
      ys = [top..(top+height-1)]

  fempty :: Fabric
  fempty = Data.Map.empty

  fabric :: Claim -> Fabric
  fabric = foldl claim1 fempty . squares

  pattern :: Regex
  pattern = mkRegex "#([[:digit:]]+) @ ([[:digit:]]+),([[:digit:]]+): ([[:digit:]]+)x([[:digit:]]+)"

  createClaim :: [String] -> Maybe Claim
  createClaim [id, left, top, width, height] = Just $ Claim {
    top = read top,
    left = read left,
    width = read width,
    height = read height
  }
  createClaim _ = Nothing

  parse :: String -> Maybe Claim
  parse input = do
    matches <- matchRegex pattern input
    claim <- createClaim matches
    return claim

  toc :: Maybe Int -> String
  toc (Just i) = show i
  toc Nothing = "."


  display :: Fabric -> (Int, Int) -> String
  display fabric (width, height) =
    let
      xs = [0..width-1]
      ys = [0..height-1]
      ls = [ concat [ toc $ Data.Map.lookup (x, y) fabric | y <- ys ] | x <- xs ]
    in
      unlines ls