module Solver where

import           Data.List   (nub)
import qualified Data.List   as List (foldr)
import           Data.Map    (Map, alter, elems, empty, lookup, unionWith)
import           Data.Maybe  (catMaybes)
import           Debug.Trace
import qualified Parse

solveA :: [String] -> String
solveA =
  show . countOverlaps . combine . map claimFabric . catMaybes . map parse

solveB :: [String] -> String
solveB = findNonOverlapping . combine . map claimFabric . catMaybes . map parse

data Claim =
  Claim
    { id'    :: String
    , top    :: Int
    , left   :: Int
    , width  :: Int
    , height :: Int
    }
  deriving (Show, Eq)

type Square = (Int, Int)

type Id = String

newtype Fabric =
  Fabric (Map Square [Id])
  deriving (Show, Eq)

countOverlaps :: Fabric -> Int
countOverlaps = length . filter (\c -> length c > 1) . elems . extract

findNonOverlapping :: Fabric -> String
findNonOverlapping fabric =
  let findOverlapping = nub . concat . filter ((<) 1 . length) . elems . extract
      findAll = nub . concat . elems . extract
      overlapped = findOverlapping fabric
      allIds = findAll fabric
      isOutlier x = notElem x overlapped
      outliers = filter isOutlier allIds
   in head outliers

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
inc i Nothing   = Just [i]
inc i (Just i') = Just (i : i')

claimSquare :: Id -> Fabric -> Square -> Fabric
claimSquare i (Fabric f) square = Fabric (alter (inc i) square f)

squares :: Claim -> [Square]
squares (Claim {top = t, left = l, width = w, height = h}) = sqs
  where
    xs = [l .. (l + w - 1)]
    ys = [t .. (t + h - 1)]
    sqs = [(x, y) | y <- ys, x <- xs]

claimFabric :: Claim -> Fabric
claimFabric claim =
  foldl (claimSquare $ id' claim) (Fabric empty) $ squares claim

parse :: String -> Maybe Claim
parse = Parse.extract createClaim . getClaimParts

getClaimParts :: String -> Parse.Parser String
getClaimParts input =
  Parse.drop '#' input >>= Parse.digits >>= Parse.drop ' ' >>= Parse.drop '@' >>=
  Parse.drop ' ' >>=
  Parse.digits >>=
  Parse.drop ',' >>=
  Parse.digits >>=
  Parse.drop ':' >>=
  Parse.drop ' ' >>=
  Parse.digits >>=
  Parse.drop 'x' >>=
  Parse.digits

createClaim :: [String] -> Maybe Claim
createClaim [i, l, t, w, h] =
  Just $
  Claim {id' = i, top = read t, left = read l, width = read w, height = read h}
createClaim input =
  trace ("failed to create claim from input: " ++ show input) Nothing

display :: Fabric -> (Int, Int) -> String
display fabric (w, h) =
  let toc (Just [i])     = i
      toc (Just (_:_:_)) = "X"
      toc (Just [])      = "."
      toc Nothing        = "."
      xs = [0 .. w - 1]
      ys = [0 .. h - 1]
      ls =
        [ concat [toc $ Data.Map.lookup (x, y) (extract fabric) | y <- ys]
        | x <- xs
        ]
   in unlines ls
