module Dec6 where

  import qualified Parse
  import Data.Map (Map, (!))
  import Data.Maybe
  import qualified Data.Map as Map (fromList, keys, elems, lookup, insert, map, mapMaybe)
  import qualified Data.List as List (minimumBy, maximumBy, nub, filter, map)

  solveA :: [String] -> String
  solveA input = show $ sizeOf g $ largestArea g
    where
      cg = cogrid $ copoints input
      g = removeInfinites $ markAreas cg $ grid $ points input

  solveB :: Int -> [String] -> String
  solveB limit input = output
    where
      cg = cogrid $ copoints input
      s = size (CG cg)
      marked = markWithin cg s limit
      output = show $ sizeOf marked 'X'

  type Size = (Int,Int)
  type Coord = (Int,Int)
  coord :: String -> Coord
  coord input = (x,y)
   where
    parsed = Parse.digits input >>= Parse.drop ',' >>= Parse.drop ' ' >>= Parse.digits
    parts = Parse.getParts parsed
    [x,y] = map (read :: String -> Int) parts

  type Point = (Coord, [Char])
  points :: [String] -> [Point]
  points = flip zip cs . map coord
    where cs = map pure ['A'..]

  newtype Grid = Grid (Map Coord [Char]) deriving (Eq)
  grid :: [Point] -> Grid
  grid = Grid . Map.fromList

  type Copoint = (Char, Coord)
  copoints :: [String] -> [Copoint]
  copoints = zip ['A'..] . map coord

  newtype Cogrid = Cogrid (Map Char Coord) deriving (Eq)
  cogrid :: [Copoint] -> Cogrid
  cogrid = Cogrid . Map.fromList

  instance Show Grid where
    show g = showGrid (size (G g)) g

  showGrid :: Size -> Grid -> String
  showGrid (maxX, maxY) (Grid g) = s
    where
      at x y =
        case Map.lookup (x,y) g of
          Just c
            | c == [head c] -> head c
            | otherwise -> 'X'
          Nothing -> '.'
      line y = [ at x y | x <- [0..maxX] ]
      s = unlines [ line y | y <- [0..maxY] ]

  showClosestTo :: Size -> Char -> Grid -> String
  showClosestTo (x,y) c (Grid g) = s
    where
      at x' y' =
        case Map.lookup (x',y') g of
          Just c'
           | c' == [c] -> c
           | otherwise -> '.'
          Nothing -> '.'
      line y' = [ at x' y' | x' <- [0..x]]
      s = unlines [ line y' | y' <- [0..y]]

  data GCG = G Grid | CG Cogrid
  size :: GCG -> Size
  size (G (Grid g)) = ((+) 1 $ maximum $ map fst $ Map.keys g, maximum $ map snd $ Map.keys g)
  size (CG (Cogrid cg)) = (((+) 1 $ maximum $ map fst $ Map.elems cg, maximum $ map snd $ Map.elems cg))

  distance :: Coord -> Coord -> Int
  distance (x,y) (x',y') = abs (x - x') + abs (y - y')

  closerTo :: Coord -> Coord -> Coord -> Ordering
  closerTo target a b
    | distance target a < distance target b = LT
    | distance target a > distance target b = GT
    | otherwise = EQ

  closest :: Coord -> Grid -> [Char]
  closest c (Grid g) = (!) g $ List.minimumBy (closerTo c) $ Map.keys g

  toBeat :: Grid -> Coord -> [Char]
  toBeat (Grid g) c =
    case Map.lookup c g of
      Just cs -> cs
      Nothing -> []

  beats :: Cogrid -> Coord -> Coord -> Char -> Ordering
  beats (Cogrid g) target c = (closerTo target c . (!) g)

  -- nuvarande karta -> usprunglig karta -> nuvarande koordinat -> kandidat -> ny karta
  updateCoord :: Cogrid -> Char -> Grid -> Coord -> Grid
  updateCoord (Cogrid g') candidate (Grid g) here = Grid $ Map.insert here winners g
    where
      winners = case toBeat (Grid g) here of
        [] -> [candidate]
        b:bs
          | elem candidate (b:bs) -> b:bs
          | otherwise -> case closerTo here (g' ! candidate) (g' ! b) of
            LT -> candidate:[]
            EQ -> candidate:b:bs
            GT -> b:bs

  markClosestTo :: Cogrid -> Grid -> Char -> Grid
  markClosestTo cg g candidate = g'
    where
      (maxX, maxY) = size (CG cg)
      allcoords = [(x,y) | y <- [0..maxY], x <- [0..maxX]]
      folder = updateCoord cg candidate
      g' = foldl folder g allcoords

  markAreas :: Cogrid -> Grid -> Grid
  markAreas cg g = g'
    where
      allchars = case cg of (Cogrid cg') -> Map.keys cg'
      folder = markClosestTo cg
      g' = foldl folder g allchars

  unmark :: Grid -> Char -> Grid
  unmark (Grid g) c = Grid g'
    where
      forget c'
        | c `elem` c' = ['.']
        | otherwise = c'
      g' = Map.map forget g

  infinites :: Grid -> [Char]
  infinites (Grid g) = List.nub $ concat $ filter ((==) 1 . length) $ catMaybes $ map (flip Map.lookup g) edges
    where
      (x,y) = size (G (Grid g))
      upper = [ (x',0) | x' <- [0..x]]
      lower = [ (x',y) | x' <- [0..x]]
      left  = [ (0,y') | y' <- [0..y]]
      right = [ (x,y') | y' <- [0..y]]
      edges = List.nub $ upper ++ lower ++ left ++ right
  
  removeInfinites :: Grid -> Grid
  removeInfinites g = foldl unmark g (infinites g)

  single :: [Char] -> Maybe Char
  single [a] = Just a
  single _ = Nothing

  compareAreas :: Grid -> Char -> Char -> Ordering
  compareAreas g a b = compare sa sb
    where
      sa = sizeOf g a
      sb = sizeOf g b

  largestArea :: Grid -> Char
  largestArea (Grid g) = List.maximumBy (compareAreas (Grid g)) $ List.filter ((/=) '.') $ List.nub $ Map.elems $ Map.mapMaybe single g

  sizeOf :: Grid -> Char -> Int
  sizeOf (Grid g) c = length $ filter ((==) c) $ Map.elems $ Map.mapMaybe single g

  totalDistance :: Cogrid -> Coord -> Int
  totalDistance (Cogrid cg) c = sum $ map (distance c) $ Map.elems cg

  markWithin :: Cogrid -> Size -> Int -> Grid
  markWithin cg (x,y) threshold = g
    where
      ps = [(x',y') | x' <- [0..x], y' <- [0..y]]
      marker d
        | d < threshold = 'X'
        | otherwise = '.'
      markers = List.map (pure . marker . (totalDistance cg)) ps
      g = Grid $ Map.fromList $ zip ps markers
