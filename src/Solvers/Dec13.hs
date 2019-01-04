module Solvers.Dec13 where

  import Prelude hiding (lookup)
  import Data.List (find, sortBy, groupBy)
  import Data.List.Unique (repeated)
  import GHC.Exts (the)
  import Data.Map (Map, insert, empty, mapMaybe, keys, lookup, fromList, toList, adjust)
  import Data.Maybe (fromJust, isJust, fromMaybe)
  import Control.Applicative ((<|>))
  import Data.Function (on)

  import Simulation (simulateUntil)
  import Utils.List (maximums, readingOrder, hasDuplicates)

  import Debug.Trace
  tpaint :: Tracks -> [Car] -> [Car]
  tpaint tracks cars = trace (paint tracks cars) cars

  type Position = (Int, Int)
  data Track = Horizontal | Vertical | ForwardSlash | Backslash | Intersection deriving (Show, Eq)
  type Tracks = Map Position Track
  data Direction = North | South | East | West deriving (Show, Eq)
  data Turn = L | R | S deriving (Show, Eq)

  type Car = (Position, Direction, Turn)
  position :: Car -> Position
  position (p,_,_) = p
  facing :: Car -> Direction
  facing (_,d,_) = d
  type Input = (Tracks, [Car])

  solveA :: [String] -> String
  solveA input = out . findCollision . head . simulateUntil 10000 hasCollision tick $ initial
    where
      out (x,y) = show x ++ "," ++ show y
      (tracks, initial) = parse input

      tick :: [Car] -> [Car]
      tick cars = moveAll cars'
        where
          cars' = sortBy (readingOrder `on` position) cars
          moveAll [] = []
          moveAll (c:cs)
            | hasCollision (c:cs) = (c:cs)
            | otherwise = move tracks c : moveAll cs

  move :: Tracks -> Car -> Car
  move tracks (pos, f, t) = (pos', f', t')
    where
      track' = trackAt tracks pos'
      pos' = step pos f
      f' = rotate tracks pos' f t
      t' = turn track' t

  solveB :: [String] -> String
  solveB input = out . position . the . head . simulateUntil 100000 oneCarLeft (tickB tracks) $ initial
    where
      out (x,y) = show x ++ "," ++ show y
      (tracks, initial) = parse input

  tickB :: Tracks -> [Car] -> [Car]
  tickB tracks = fmap snd . toList . moveAll 0 . fromList . zip [0..] . sortBy (readingOrder `on` position)
    where
      moveAll :: Int -> Map Int Car -> Map Int Car
      moveAll i cs
        | (maximum . keys $ cs) < i = cs
        | otherwise = moveAll (i+1) (move1 i cs)
        where
          move1 :: Int -> Map Int Car -> Map Int Car
          move1 i' = dedupe . adjust (move tracks) i'

  dedupe :: Map Int Car -> Map Int Car
  dedupe = fromList . concat . filter ((==) 1 . length) . groups . toList
    where
      groups :: [(Int, Car)] -> [[(Int,Car)]]
      groups = groupBy ((==) `on` pos)
        where
          pos (_, (p, _, _)) = p

  trackAt :: Tracks -> Position -> Track
  trackAt tracks pos =
    case lookup pos tracks of
      Just t -> t
      Nothing -> error $ "Missing track at " ++ show pos

  oneCarLeft :: [Car] -> Bool
  oneCarLeft = (==) 1 . length

  hasCollision :: [Car] -> Bool
  hasCollision = hasDuplicates . fmap position

  findCollision :: [Car] -> Position
  findCollision = the . repeated . fmap position

  step :: Position -> Direction -> Position
  step pos North = above   pos
  step pos South = below   pos
  step pos East  = rightOf pos
  step pos West  = leftOf  pos

  above :: Position -> Position
  above (x,y) = (x,y-1)
  below :: Position -> Position
  below (x,y) = (x,y+1)
  rightOf :: Position -> Position
  rightOf (x,y) = (x+1,y)
  leftOf :: Position -> Position
  leftOf (x,y) = (x-1,y)

  turn :: Track -> Turn -> Turn
  turn Intersection t = turnOnce t
    where
      turnOnce L = S
      turnOnce S = R
      turnOnce R = L
  turn _ t = t

  rotate :: Tracks -> Position -> Direction -> Turn -> Direction
  rotate tracks pos d t = _rotate tr d t
    where
      tr = trackAt tracks pos
      _rotate Vertical North _ = North
      _rotate Vertical South _ = South
      _rotate Horizontal West _ = West
      _rotate Horizontal East _ = East
      _rotate ForwardSlash North _ = East
      _rotate ForwardSlash West _ = South
      _rotate ForwardSlash East _ = North
      _rotate ForwardSlash South _ = West
      _rotate Backslash North _ = West
      _rotate Backslash East _ = South
      _rotate Backslash South _ = East
      _rotate Backslash West _ = North
      _rotate Intersection North L = West
      _rotate Intersection West L = South
      _rotate Intersection South L = East
      _rotate Intersection East L = North
      _rotate Intersection North R = East
      _rotate Intersection West R = North
      _rotate Intersection South R = West
      _rotate Intersection East R = South
      _rotate Intersection f S = f
      _rotate _ fc _ = error $ "unsupported track/direction/turn combo at " ++ show pos ++ ": " ++ show tr ++ "/" ++ show fc ++ "/" ++ show t

  rotateOnce :: Direction -> Turn -> Direction
  rotateOnce North L = West
  rotateOnce West  L = South
  rotateOnce South L = East
  rotateOnce East  L = North
  rotateOnce North R = East
  rotateOnce West  R = North
  rotateOnce South R = West
  rotateOnce East  R = South
  rotateOnce f     S = f

  paint :: Tracks -> [Car] -> String
  paint tracks cars = unlines $ fmap paintLine [0..ymax]
    where
      (xmax,ymax) = maximums $ keys tracks
      paintLine y = fmap (paintCoord y) [0..xmax]
      paintCoord y x = fromMaybe ' ' $ paintCar (x,y) <|> paintTrack (x,y)

      paintCar pos = do
        c <- find ((==) pos . position) cars
        return $ case facing c of
          North -> '^'
          South -> 'v'
          East  -> '>'
          West  -> '<'

      paintTrack pos = do
        t <- lookup pos tracks
        return $ case t of
          Vertical -> '|'
          Horizontal -> '-'
          ForwardSlash -> '/'
          Backslash -> '\\'
          Intersection -> '+'

  parse :: [String] -> Input
  parse = foldMap (uncurry parseLine) . zip [0..]
    where
      parseLine y = foldMap (parseCoord y) . zip [0..]
      parseCoord y (x, c) = (tracks', cars')
        where
          tracks' = mapMaybe id $ insert (x,y) (trackFor c) empty
          cars' = fmap (initCarAt (x,y)) . fmap fromJust . filter isJust $ [carFor c]

          trackFor '|' = Just Vertical
          trackFor '-' = Just Horizontal
          trackFor '/' = Just ForwardSlash
          trackFor '\\' = Just Backslash
          trackFor '+' = Just Intersection
          trackFor 'v' = Just Vertical
          trackFor '^' = Just Vertical
          trackFor '<' = Just Horizontal
          trackFor '>' = Just Horizontal
          trackFor _ = Nothing

          carFor '^' = Just North
          carFor '<' = Just West
          carFor '>' = Just East
          carFor 'v' = Just South
          carFor _ = Nothing

          initCarAt pos f = (pos, f, L)
