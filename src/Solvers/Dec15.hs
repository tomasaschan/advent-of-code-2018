module Solvers.Dec15 where

import Prelude hiding (round)

import Data.List (sortBy)
import Data.Map (Map, (!))
import qualified Data.Map as Map (lookup, fromList, toList, keys, member)
import Data.Set (Set)
import qualified Data.Set as Set (fromList)
import Data.Maybe (catMaybes, isJust, fromMaybe)
import Data.Function (on)

import Data.Pathfinding
import Utils.Foldable (maximums)
import Utils.List (readingOrder)

type Coord = (Int,Int)
data Terrain = Wall | Cavern deriving (Show, Eq)
data Race = Elf | Goblin deriving (Show, Eq)
data Unit = Unit Race Coord deriving (Show, Eq)
position :: Unit -> Coord
position (Unit _ coord) = coord
race :: Unit -> Race
race (Unit r _) = r
movedTo :: Coord -> Unit -> Unit
movedTo c (Unit r _) = Unit r c
type Dungeon = Map Coord Bool
isOpen :: Dungeon -> Coord -> Bool
isOpen dungeon c = dungeon ! c
size :: Dungeon -> Coord
size = maximums . Map.keys
data World = World Dungeon [Unit] deriving (Show, Eq)

data Err = Unknown String deriving (Show, Eq)

data Problem = A | B | Both deriving (Show, Eq)

initial :: Problem -> [String] -> World
initial problem input = World dungeon units
  where
    dungeon = parseDungeon input
    units = parseUnits problem input

play :: World -> [World]
play (World dungeon units) = fmap (repack dungeon) . take 20 . iterate (round dungeon) $ units
  where repack d u = World d u

round :: Dungeon -> [Unit] -> [Unit]
round dungeon = turn dungeon [] . sortBy (readingOrder `on` position)

turn :: Dungeon -> [Unit] -> [Unit] -> [Unit]
turn _       units []              = units
turn dungeon prev (this:next) = turn dungeon (this':prev') next'
  where (prev', this', next') = makeMove dungeon prev this next

makeMove :: Dungeon -> [Unit] -> Unit -> [Unit] -> ([Unit], Unit, [Unit])
makeMove dungeon prev this next = (prev', this', next')
  where
    others = prev ++ next
    targets = concatMap (inRangeOf . position) . filter (enemyOf this) $ others

    prev' = prev
    this' = move dungeon targets others this
    next' = next

move :: Dungeon -> [Coord] -> [Unit] -> Unit -> Unit
move dungeon targets others this =
  if position this `elem` targets then this
  else
    case shortestPathReadingOrder (walkable (World dungeon others)) inRangeOf (position this) targets of
      Just (here:there:_) -> movedTo there this
      Nothing -> this

enemyOf :: Unit -> Unit -> Bool
enemyOf (Unit Elf _) (Unit Goblin _) = True
enemyOf (Unit Goblin _) (Unit Elf _) = True
enemyOf _ _ = False

walkable :: World -> (Coord -> Bool)
walkable (World dungeon units) = \c ->
  let
    open = fromMaybe False $ Map.lookup c dungeon
    occupied = c `elem` fmap position units
  in open && not occupied

inRangeOf :: Coord -> [Coord]
inRangeOf c = fmap (add c) [(0,1),(-1,0),(1,0),(0,-1)]
  where add (x,y) (x',y') = (x+x',y+y')

parseUnits :: Problem -> [String] -> [Unit]
parseUnits _ = foldMap coupleLine . zip [0..] . fmap (zip [0..])
  where
    coupleLine (y, l) = catMaybes $ fmap (couplePoint y) l
    couplePoint y (x, 'E') = Just $ Unit Elf (x,y)
    couplePoint y (x, 'G') = Just $ Unit Goblin (x,y)
    couplePoint _ _ = Nothing

parseDungeon :: [String] -> Dungeon
parseDungeon = Map.fromList . foldMap coupleLine . zip [0..] . fmap (zip [0..])
  where
    coupleLine (y, l) = fmap (couplePoint y) l
    couplePoint y (x,c) = ((x,y), parseTerrain c)
    parseTerrain '#' = False -- Wall
    parseTerrain '.' = True  -- Cavern
    parseTerrain 'E' = True  -- Cavern
    parseTerrain 'G' = True  -- Cavern
    parseTerrain c = error $ "Invalid input character: " ++ show c
