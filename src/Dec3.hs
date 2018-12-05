module Dec3 where

  import Data.Map (Map, alter, empty, toList, lookup, unionWith, elems)
  import qualified Data.List as List (foldr)
  import Data.Maybe (catMaybes)
  import qualified Parse
  import Debug.Trace

  solveA :: [String] -> String
  solveA = show . countOverlaps . combine . map claimFabric . catMaybes . map parse

  data Claim = Claim {
    id' :: String,
    top :: Int,
    left :: Int,
    width :: Int,
    height :: Int
  } deriving (Show, Eq)

  type Square = (Int, Int)
  type Id = String
  newtype Fabric = Fabric (Map Square [Id]) deriving (Show, Eq)

  countOverlaps :: Fabric -> Int
  countOverlaps = length . filter (\c -> length c > 1) . elems . extract

  extract :: Fabric -> Map Square [Id]
  extract (Fabric d) = d

  join :: Fabric -> Fabric -> Fabric
  join (Fabric a) (Fabric b) = Fabric (unionWith (++) a b)

  instance Semigroup Fabric where
    (<>) = join

  instance Monoid Fabric where
    mempty = Fabric empty
    mappend = (<>)

  combine :: [Fabric] -> Fabric
  combine = List.foldr join mempty

  inc :: Id -> Maybe [Id] -> Maybe [Id]
  inc id' Nothing = Just [id']
  inc id' (Just id'') = Just (id':id'')

  claimSquare :: Id -> Fabric -> Square -> Fabric
  claimSquare id' (Fabric f) square = Fabric (alter (inc id') square f)

  squares :: Claim -> [Square]
  squares (Claim { top = top, left = left, width = width, height = height }) = squares
    where
      xs = [left..(left+width-1)]
      ys = [top..(top+height-1)]
      squares = [(x,y) | y <- ys, x <- xs]

  claimFabric :: Claim -> Fabric
  claimFabric claim = foldl (claimSquare $ id' claim) (Fabric empty) $ squares claim

  parse :: String -> Maybe Claim
  parse = createClaim . Parse.getParts . getClaimParts

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

  display :: Fabric -> (Int, Int) -> String
  display fabric (width, height) =
    let
      toc (Just [i]) = i
      toc (Just (i:j:_)) = "X"
      toc Nothing = "."
      xs = [0..width-1]
      ys = [0..height-1]
      ls = [ concat [ toc $ Data.Map.lookup (x, y) (extract fabric) | y <- ys ] | x <- xs ]
    in
      unlines ls