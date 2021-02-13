module Solver where

import           Prelude                 hiding ( round )

import           Data.Bifunctor                 ( second )
import           Data.Foldable.Extended         ( maximums
                                                , toList
                                                )
import           Data.Function                  ( on )
import           Data.List.Extended             ( readingOrder
                                                , sortBy
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
                                                ( fromList
                                                , keys
                                                , lookup
                                                )
import           Data.Maybe                     ( mapMaybe
                                                , fromMaybe
                                                , listToMaybe
                                                )
import           Data.Pathfinding
import           Data.Sequence                  ( Seq((:<|)) )
import qualified Data.Sequence                 as Seq
import qualified Data.Set                      as Set
                                                ( fromList )
import           Data.Tuple                     ( swap )

import           Debug.Trace

printWithLabel :: Show a => String -> a -> a
printWithLabel lbl x = trace (lbl ++ ": " ++ show x) x

type Coord = (Int, Int)

type Turns = Int

data Terrain
  = Wall
  | Cavern
  deriving (Show, Eq)

data Race
  = Elf
  | Goblin
  deriving (Show, Eq)

type Attack = Int

type HP = Int

data Unit =
  Unit Race Coord Attack HP
  deriving (Show, Eq)

position :: Unit -> Coord
position (Unit _ coord _ _) = coord

race :: Unit -> Race
race (Unit r _ _ _) = r

is :: Race -> Unit -> Bool
is r = (==) r . race

hitPoints :: Unit -> HP
hitPoints (Unit _ _ _ hp) = hp

power :: Unit -> Attack
power (Unit _ _ a _) = a

movedTo :: Coord -> Unit -> Unit
movedTo c (Unit r _ a hp) = Unit r c a hp

type Dungeon = Map Coord Terrain

size :: Dungeon -> Coord
size = maximums . Map.keys

type World = (Dungeon, [Unit])

newtype Err =
  Unknown String
  deriving (Show, Eq)

data Problem
  = A
  | B Int
  deriving (Show, Eq)

initial :: Int -> [String] -> World
initial elfPower input =
  let dungeon = parseDungeon input
      units   = parseUnits elfPower input
  in  (dungeon, units)

summarize :: ([[Unit]], Int) -> (Int, Int, Int, Int)
summarize (history, turns) = (turns, totalHP, outcome', elvesAlive)
 where
  totalHP    = totalHitPoints . last $ history
  outcome'   = turns * totalHP
  elvesAlive = length . filter (is Elf) . last $ history

outcome :: ([[Unit]], Int) -> Int
outcome final = let (_, _, outcome', _) = summarize final in outcome'

solveA :: [String] -> String
solveA = show . outcome . uncurry play . initial 3

solveB :: [String] -> String
solveB = show . findVictory 4

findVictory :: Int -> [String] -> Int
findVictory pwr input =
  let (dungeon, units)         = initial pwr input
      elves                    = length . filter (is Elf) $ units
      (_, _, outcome', elves') = summarize $ play dungeon units
  in  if elves == elves' then outcome' else findVictory (pwr + 1) input

play :: Dungeon -> [Unit] -> ([[Unit]], Turns)
play dungeon units = turn dungeon [] [] units 0

totalHitPoints :: [Unit] -> Int
totalHitPoints = sum . fmap hitPoints

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ []       = []
takeWhileInclusive p (x : xs) = x : if p x then takeWhileInclusive p xs else []

gameOver :: [Unit] -> Bool
gameOver units = all ((==) Goblin . race) units || all ((==) Elf . race) units

inReadingOrder :: [Unit] -> [Unit]
inReadingOrder = sortBy (readingOrder `on` position)

turn :: Dungeon -> [[Unit]] -> [Unit] -> [Unit] -> Turns -> ([[Unit]], Turns)
turn dungeon history moved [] turns = turn dungeon
                                           history'
                                           []
                                           (inReadingOrder moved)
                                           (turns + 1)
  where history' = moved : history
turn dungeon history moved (this : waiting) turns
  | gameOver all' = (reverse (all' : history), turns)
  | otherwise     = turn dungeon history (this' : moved') waiting' turns
 where
  all'                      = this : waiting ++ moved
  (moved', this', waiting') = makeMove dungeon moved this waiting

makeMove :: Dungeon -> [Unit] -> Unit -> [Unit] -> ([Unit], Unit, [Unit])
makeMove dungeon prev this next = (prev', this', next')
 where
  others         = prev ++ next
  targets = concatMap (inRangeOf . position) . filter (enemyOf this) $ others
  this'          = move dungeon others targets this
  (prev', next') = attack this' prev next

move :: Dungeon -> [Unit] -> [Coord] -> Unit -> Unit
move dungeon units targets this = if position this `elem` targets
  then this
  else case shortestPathReadingOrder dungeon units (position this) targets of
    Just p  -> let (_ : there : _) = p in movedTo there this
    Nothing -> this

attack :: Unit -> [Unit] -> [Unit] -> ([Unit], [Unit])
attack attacker prev next = fromMaybe (prev, next) $ do
  target <-
    listToMaybe
    .  sortBy (compare `on` sortOrder)
    .  filter (flip elem (inRangeOf $ position attacker) . position)
    .  filter (enemyOf attacker)
    $  prev
    ++ next
  let tp  = position target
  let pwr = power attacker
  return (allAttacked tp pwr prev, allAttacked tp pwr next)
 where
  allAttacked tp pwr = filter ((> 0) . hitPoints) . fmap (attacked pwr tp)
  sortOrder (Unit _ (x, y) _ hp) = (hp, y, x)
  attacked pwr tp (Unit r p a hp) | p == tp   = Unit r p a (hp - pwr)
                                  | otherwise = Unit r p a hp

enemyOf :: Unit -> Unit -> Bool
enemyOf (Unit Elf    _ _ _) (Unit Goblin _ _ _) = True
enemyOf (Unit Goblin _ _ _) (Unit Elf    _ _ _) = True
enemyOf _                   _                   = False

walkable :: Dungeon -> [Unit] -> (Coord -> Bool)
walkable dungeon units c =
  let terrain  = fromMaybe Wall $ Map.lookup c dungeon
      occupied = c `elem` fmap position units
  in  terrain == Cavern && not occupied

inRangeOf :: Coord -> [Coord]
inRangeOf c = fmap (add c) [(0, 1), (-1, 0), (1, 0), (0, -1)]
  where add (x, y) (x', y') = (x + x', y + y')

shortestPathReadingOrder
  :: Dungeon -> [Unit] -> Coord -> [Coord] -> Maybe [Coord]
shortestPathReadingOrder dungeon units source targets =
  let prio :: Seq Coord -> (Int, Coord, Seq Coord)
      prio path = (l, t, p)
         where
          l = Seq.length path
          t = let h :<| _ = path in swap h
          p = fmap swap . Seq.reverse $ path
  in  do
        path <- bfsMapTraversal (walkable dungeon units)
                                (Seq.fromList . inRangeOf)
                                prio
                                source
                                (Set.fromList targets)
        return $ toList path
    -- we want to prioritize first by length, then by reading order on the first differing position
    -- tuples and lists are sorted lexically, so two lists of (y,x) pairs will sort according to
    -- the first one that differs, and (y,x) sorts in reading order

parseUnits :: Int -> [String] -> [Unit]
parseUnits pwr = foldMap coupleLine . zip [0 ..] . fmap (zip [0 ..])
 where
  coupleLine (y, l) = mapMaybe (couplePoint y) l
  couplePoint y (x, 'E') = Just $ Unit Elf (x, y) pwr 200
  couplePoint y (x, 'G') = Just $ Unit Goblin (x, y) 3 200
  couplePoint _ _        = Nothing

parseDungeon :: [String] -> Dungeon
parseDungeon lines' =
  let indexed = foldMap coupleLine . zip [0 ..] . fmap (zip [0 ..]) $ lines'
      parsed  = fmap (second parseTerrain) indexed
  in  Map.fromList parsed
 where
  coupleLine :: (Int, [(Int, Char)]) -> [((Int, Int), Char)]
  coupleLine (y, l) = fmap (couplePoint y) l
  couplePoint :: Int -> (Int, Char) -> ((Int, Int), Char)
  couplePoint y (x, c) = ((x, y), c)
  parseTerrain :: Char -> Terrain
  parseTerrain '#' = Wall
  parseTerrain '.' = Cavern
  parseTerrain 'E' = Cavern
  parseTerrain 'G' = Cavern
  parseTerrain c   = error $ "Invalid input character: " ++ show c
