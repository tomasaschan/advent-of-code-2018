module Solvers.Dec15 where

import Data.List (sortBy)
import Data.Map (Map, fromList, toList, keys)
import Data.Maybe (catMaybes)
import Data.Function (on)

import Utils.Foldable (maximums)
import Utils.List (readingOrder)

type Coord = (Int, Int)
data Terrain = Wall | Cavern deriving (Show, Eq)
data Race = Elf | Goblin deriving (Show, Eq)
data Unit = Unit Race Coord deriving (Show, Eq)
position :: Unit -> Coord
position (Unit _ coord) = coord
type Dungeon = Map Coord Terrain
toList :: Dungeon -> [(Coord, Terrain)]
toList = Data.Map.toList
size :: Dungeon -> Coord
size = maximums . keys
data World = World Dungeon [Unit] deriving (Show, Eq)

data Err = Unknown String deriving (Show, Eq)

data Problem = A | B | Both deriving (Show, Eq)

initial :: Problem -> [String] -> World
initial problem input = World dungeon units
  where
    dungeon = parseDungeon input
    units = parseUnits problem input

round :: Dungeon -> [Unit] -> [Unit]
round dungeon = turn dungeon [] . sortBy (readingOrder `on` position)

turn :: Dungeon -> [Unit] -> [Unit] -> [Unit]
turn _       units []              = units
turn dungeon moved (toMove:inLine) = turn dungeon (toMove:moved) inLine

parseUnits :: Problem -> [String] -> [Unit]
parseUnits _ = foldMap coupleLine . zip [0..] . fmap (zip [0..])
  where
    coupleLine (y, l) = catMaybes $ fmap (couplePoint y) l
    couplePoint y (x, 'E') = Just $ Unit Elf (x,y)
    couplePoint y (x, 'G') = Just $ Unit Goblin (x,y)
    couplePoint _ _ = Nothing

parseDungeon :: [String] -> Dungeon
parseDungeon = fromList . foldMap coupleLine . zip [0..] . fmap (zip [0..])
  where
    coupleLine (y, l) = fmap (couplePoint y) l
    couplePoint y (x,c) = ((x,y), parseTerrain c)
    parseTerrain '#' = Wall
    parseTerrain '.' = Cavern
    parseTerrain 'E' = Cavern
    parseTerrain 'G' = Cavern
    parseTerrain c = error $ "Invalid input character: " ++ show c
