module Solvers.Dec15 where

import Prelude hiding (round)

import Data.List (sortBy)
import Data.Map (Map, (!))
import qualified Data.Map as Map (lookup, fromList, toList, keys, member, empty)
import Data.Set (Set)
import qualified Data.Set as Set (fromList)
import Data.Maybe (catMaybes, isJust, fromMaybe, listToMaybe)
import Data.Function (on)
import Data.Bifunctor (bimap)

import Data.Pathfinding
import Utils.Foldable (maximums)
import Utils.List (readingOrder)

import Debug.Trace
printWithLabel :: Show a => String -> a -> a
printWithLabel lbl x = trace (lbl ++ ": " ++ show x) x

type Coord = (Int,Int)
type Turns = Int
data Terrain = Wall | Cavern deriving (Show, Eq)
data Race = Elf | Goblin deriving (Show, Eq)
type Attack = Int
type HP = Int
data Unit = Unit Race Coord Attack HP deriving (Show, Eq)
position :: Unit -> Coord
position (Unit _ coord _ _) = coord
race :: Unit -> Race
race (Unit r _ _ _) = r
hitPoints (Unit _ _ _ hp) = hp
power :: Unit -> Attack
power (Unit _ _ a _) = a
movedTo :: Coord -> Unit -> Unit
movedTo c (Unit r _ a hp) = Unit r c a hp
type Dungeon = Map Coord Terrain
size :: Dungeon -> Coord
size = maximums . Map.keys
data World = World Dungeon [Unit] deriving (Show, Eq)

data Err = Unknown String deriving (Show, Eq)

data Problem = A | B | Both deriving (Show, Eq)

initial :: Problem -> [String] -> World
initial problem input =
  let
    dungeon = parseDungeon input
    units = parseUnits problem input
  in World dungeon units

solveA :: ([[Unit]], Int) -> (Int,Int,Int)
solveA (history, turns) = (turns, totalHP, outcome)
  where
    totalHP = totalHitPoints . last $ history
    outcome = turns * totalHP

play :: Dungeon -> [Unit] -> ([[Unit]], Turns)
play dungeon units = turn dungeon [] [] units 0

totalHitPoints :: [Unit] -> Int
totalHitPoints = sum . fmap hitPoints

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs else []

gameOver :: [Unit] -> Bool
gameOver units = all ((==) Goblin . race) units || all ((==) Elf . race) units

inReadingOrder :: [Unit] -> [Unit]
inReadingOrder = sortBy (readingOrder `on` position)

turn :: Dungeon -> [[Unit]] -> [Unit] -> [Unit] -> Turns -> ([[Unit]], Turns)
turn dungeon history moved [] turns = turn dungeon history' [] (inReadingOrder moved) (turns + 1)
  where history' = moved:history
turn dungeon history moved (this:waiting) turns
  | gameOver all = (reverse (all:history), turns)
  | otherwise = turn dungeon history (this':moved') waiting' turns
    where
      all = this:waiting ++ moved
      (moved', this', waiting') = makeMove dungeon moved this waiting

makeMove :: Dungeon -> [Unit] -> Unit -> [Unit] -> ([Unit], Unit, [Unit])
makeMove dungeon prev this next = (prev', this', next')
  where
    others = prev ++ next
    targets = concatMap (inRangeOf . position) . filter (enemyOf this) $ others
    canMoveTo = walkable (World dungeon others)
    this' = move canMoveTo targets this
    (prev', next') = attack this' prev next

move :: (Coord -> Bool) -> [Coord] -> Unit -> Unit
move canMoveTo targets this =
  if position this `elem` targets then this
  else
    case shortestPathReadingOrder canMoveTo inRangeOf (position this) targets of
      Just p -> let (here:there:_) = p in movedTo there this
      Nothing -> this

attack :: Unit -> [Unit] -> [Unit] -> ([Unit], [Unit])
attack attacker prev next = fromMaybe (prev,next) $ do
  target <- listToMaybe . sortBy (compare `on` sortOrder) . filter (flip elem (inRangeOf $ position attacker) . position) . filter (enemyOf attacker) $ prev ++ next
  let tp = position target
  let pwr = power attacker
  return (allAttacked tp pwr prev, allAttacked tp pwr next)
    where
      allAttacked tp pwr = filter ((>0) . hitPoints) . fmap (attacked pwr tp)
      sortOrder (Unit _ (x,y) _ hp) = (hp,y,x)
      attacked pwr tp (Unit r p a hp)
        | p == tp   = Unit r p a (hp-pwr)
        | otherwise = Unit r p a hp


enemyOf :: Unit -> Unit -> Bool
enemyOf (Unit Elf _ _ _) (Unit Goblin _ _ _) = True
enemyOf (Unit Goblin _ _ _) (Unit Elf _ _ _) = True
enemyOf _ _ = False

walkable :: World -> (Coord -> Bool)
walkable (World dungeon units) = \c ->
  let
    terrain = fromMaybe Wall $ Map.lookup c dungeon
    occupied = c `elem` fmap position units
  in terrain == Cavern && not occupied

inRangeOf :: Coord -> [Coord]
inRangeOf c = fmap (add c) [(0,1),(-1,0),(1,0),(0,-1)]
  where add (x,y) (x',y') = (x+x',y+y')

parseUnits :: Problem -> [String] -> [Unit]
parseUnits _ = foldMap coupleLine . zip [0..] . fmap (zip [0..])
  where
    coupleLine (y, l) = catMaybes $ fmap (couplePoint y) l
    couplePoint y (x, 'E') = Just $ Unit Elf (x,y) 3 200
    couplePoint y (x, 'G') = Just $ Unit Goblin (x,y) 3 200
    couplePoint _ _ = Nothing

parseDungeon :: [String] -> Dungeon
parseDungeon lines =
  let
    indexed = foldMap coupleLine . zip [0..] . fmap (zip [0..]) $ lines
    parsed = fmap (bimap id parseTerrain) indexed
  in
    Map.fromList parsed
  where
    coupleLine :: (Int, [(Int, Char)]) -> [((Int, Int), Char)]
    coupleLine (y, l) = fmap (couplePoint y) l
    couplePoint :: Int -> (Int, Char) -> ((Int, Int), Char)
    couplePoint y (x,c) = ((x,y), c)

    parseTerrain :: Char -> Terrain
    parseTerrain '#' = Wall
    parseTerrain '.' = Cavern
    parseTerrain 'E' = Cavern
    parseTerrain 'G' = Cavern
    parseTerrain c = error $ "Invalid input character: " ++ show c

    untangle :: (Coord, Either Err Terrain) -> Either Err (Coord, Terrain)
    untangle (c, Left e)  = Left e
    untangle (c, Right t) = Right (c, t)

    unlist :: [Either a b] -> Either a [b]
    unlist [] = Right []
    unlist ((Right x):xs) = do
      rest <- unlist xs
      return (x:rest)
    unlist ((Left e):_) = Left e

