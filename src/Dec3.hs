module Dec3 where

  import Data.Map (Map, alter, empty, toList, lookup, unionWith, elems)
  import qualified Data.List as List (foldr)
  import Data.Maybe (catMaybes)
  import qualified Parse
  import Debug.Trace

  solveA :: [String] -> String
  solveA = show . countOverlaps . combine . map fabric . catMaybes . map parse

  data Claim = Claim {
    id' :: String,
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


  getClaimParts :: String -> Parse.Parser String
  getClaimParts input =
    Parse.drop '#' input
    >>= Parse.digits
    >>= Parse.drop ' '
    >>= Parse.drop '@'
    >>= Parse.drop ' '
    >>= Parse.digits
    >>= Parse.drop ','
    >>= Parse.digits
    >>= Parse.drop ':'
    >>= Parse.drop ' '
    >>= Parse.digits
    >>= Parse.drop 'x'
    >>= Parse.digits
  
  createClaim :: [String] -> Maybe Claim
  createClaim [id', left, top, width, height] = Just $ Claim {
    id' = id',
    top = read top,
    left = read left,
    width = read width,
    height = read height
  }
  createClaim input = trace ("failed to create claim from input: " ++ show input) Nothing

  parse :: String -> Maybe Claim
  parse = createClaim . Parse.getParts . getClaimParts

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