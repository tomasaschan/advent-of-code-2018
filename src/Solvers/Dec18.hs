module Solvers.Dec18 where

  import Data.Map (Map, insert, empty, toList, fromList)
  import Data.List (filter, elemIndex)
  import qualified Data.Map as Map (lookup)
  import Data.Maybe (catMaybes)

  data State = Open | Trees | Lumbermill deriving (Show,Eq)
  type Coord = (Int,Int)
  type CollectionArea = Map Coord State

  solveA :: [String] -> String
  solveA = show . resourceValue . simulate 10 [] . createMap . map parseRow

  solveB :: [String] -> String
  solveB = show . resourceValue . simulate 1000000000 [] . createMap . map parseRow

  parse :: Char -> Maybe State
  parse '.' = Just Open
  parse '|' = Just Trees
  parse '#' = Just Lumbermill
  parse _ = Nothing

  parseRow :: String -> [State]
  parseRow = catMaybes . map parse

  createMap :: [[State]] -> CollectionArea
  createMap states = _createMap 1 states empty
    where
      _createRow _ _ [] m = m
      _createRow x y (s:ss) m = _createRow (x+1) y ss (insert (x,y) s m)

      _createMap _ [] m = m
      _createMap y (r:rs) m = _createMap (y+1) rs (_createRow 1 y r m)

  stateAt :: CollectionArea -> Coord -> State
  stateAt m c = case Map.lookup c m of
    Just s -> s
    Nothing -> Open

  surroundings :: CollectionArea -> Coord -> [State]
  surroundings m (x,y) = map (stateAt m) [ (x+1,y+1), (x+1,y), (x+1,y-1), (x,y-1), (x-1,y-1), (x-1,y), (x-1,y+1), (x,y+1) ]

  hasAtLeast :: Eq a => Int -> a -> [a] -> Bool
  hasAtLeast n _s = (<=) n . length . filter ((==) _s)

  next :: State -> [State] -> State
  next Open s
    | hasAtLeast 3 Trees s = Trees
    | otherwise = Open
  next Trees s
    | hasAtLeast 3 Lumbermill s = Lumbermill
    | otherwise = Trees
  next Lumbermill s
    | hasAtLeast 1 Lumbermill s && hasAtLeast 1 Trees s = Lumbermill
    | otherwise = Open

  step :: CollectionArea -> CollectionArea
  step initial = fromList . map progress . toList $ initial
    where
      progress (c,s) = (c, next s (surroundings initial c))

  simulate :: Int -> [CollectionArea] -> CollectionArea -> CollectionArea
  simulate 0 _ m = m
  simulate n history m = simulate n' history' m'
    where
      (n', history', m') = case elemIndex m history of
        Just i -> (n `mod` (i + 1), [], m)
        Nothing -> (n-1, m:history, step m)

  resourceValue :: CollectionArea -> Int
  resourceValue m = lumbered * milled
    where
      states = map snd . toList $ m
      lumbered = length . filter ((==) Trees) $ states
      milled = length . filter ((==) Lumbermill) $ states

  displayState :: State -> Char
  displayState Open = '.'
  displayState Trees = '|'
  displayState Lumbermill = '#'

  displayMap :: Int -> CollectionArea -> [String]
  displayMap size m = [ map s [ (x,y) | x <- [1..size] ] | y <- [1..size] ]
    where s = displayState . stateAt m
