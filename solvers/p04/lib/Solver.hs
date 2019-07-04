module Solver where

import           Data.Function   (on)
import           Data.List       (elemIndex, map, maximumBy, nub, sort)
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as Map (alter, empty, toList)
import           Data.Maybe
import qualified Parse

solveA :: [String] -> String
solveA input = show (guard * m)
  where
    watches = getWatches input
    guard = guardWithMostMinutesAsleep watches
    m = guardsMostFrequentMinute guard watches

solveB :: [String] -> String
solveB input = show answer
  where
    watches = getWatches input
    guards = nub $ map fst watches
    mapper :: Guard -> (Guard, (Int, Int))
    mapper g = (g, (f, s))
      where
        f = guardsMostFrequentMinute g watches
        s = asleepInMinute g f watches
    answer =
      uncurry (*) $
      (id >< fst) $ maximumBy (compare `on` (snd . snd)) $ map mapper guards

type Guard = Int

data State
  = Awake
  | Asleep
  | Away
  deriving (Eq, Show)

type Watch = (Guard, [State])

data EventType
  = BeginShift Guard
  | WakeUp
  | FallAsleep
  deriving (Show, Eq)

data Event
  = Splitter
  | Event
      { month  :: Int
      , day    :: Int
      , hour   :: Int
      , minute :: Int
      , what   :: EventType
      }
  deriving (Show, Eq)

parseRow :: String -> Parse.Parser String
parseRow s =
  Parse.expect "[1518-" s >>= Parse.digits >>= Parse.expect "-" >>= Parse.digits >>=
  Parse.expect " " >>=
  Parse.digits >>=
  Parse.expect ":" >>=
  Parse.digits >>=
  Parse.expect "] " >>=
  (Parse.peek $ \c ->
     \s' ->
       case c of
         'G' -> Parse.expect "Guard #" s' >>= Parse.digits
         _   -> Parse.readRest s')

dayAfter :: String -> String
dayAfter = show . next . read
  where
    next :: Int -> Int
    next = (+) 1

buildEvent :: [String] -> Event
buildEvent [mo, dd, "23", _, e] = buildEvent [mo, dayAfter dd, "00", "00", e]
buildEvent [mo, dd, hh, mm, "wakes up"] =
  Event
    { month = read mo
    , day = read dd
    , hour = read hh
    , minute = read mm
    , what = WakeUp
    }
buildEvent [mo, dd, hh, mm, "falls asleep"] =
  Event
    { month = read mo
    , day = read dd
    , hour = read hh
    , minute = read mm
    , what = FallAsleep
    }
buildEvent [mo, dd, hh, mm, guard] =
  Event
    { month = read mo
    , day = read dd
    , hour = read hh
    , minute = read mm
    , what = BeginShift $ read guard
    }
buildEvent x = error $ "Incorrect input for parsing: " ++ show x

_insertSplitter :: Event -> [Event]
_insertSplitter e =
  case what e of
    BeginShift _ -> [Splitter, e]
    _            -> [e]

insertSplitters :: [Event] -> [Event]
insertSplitters = tail . concatMap _insertSplitter

_buildRow :: State -> Int -> [Event] -> [State]
_buildRow Away now (Event {minute = m, what = BeginShift _}:es) =
  replicate (m - now) Away ++ _buildRow Awake m es
_buildRow Awake now (Event {minute = m, what = FallAsleep}:es) =
  replicate (m - now) Awake ++ _buildRow Asleep m es
_buildRow Asleep now (Event {minute = m, what = WakeUp}:es) =
  replicate (m - now) Asleep ++ _buildRow Awake m es
_buildRow s now [] = replicate (60 - now) s
_buildRow s _ (Event {what = w}:_) =
  error $ "invalid state transition from " ++ show s ++ " to " ++ show w
_buildRow _ _ (Splitter:_) = error $ "Splitter not removed correctly"

buildRow :: [Event] -> Watch
buildRow (e:es) =
  case (what e) of
    BeginShift guard -> (guard, _buildRow Away 0 (e : es))
    _ ->
      error $
      "Watch must begin with BeginShift event (this one started with " ++
      show (what e) ++ ")"
buildRow _ = error "Watch must be built from at least one event"

buildMatrix :: [Event] -> [Watch]
buildMatrix = map buildRow . splitOn [Splitter] . insertSplitters

getWatches :: [String] -> [Watch]
getWatches = buildMatrix . map (Parse.extract buildEvent . parseRow) . sort

showMinute :: State -> String
showMinute Away   = "^"
showMinute Awake  = "."
showMinute Asleep = "#"

showRow :: Watch -> String
showRow (guard, events) = show guard ++ ": " ++ concatMap showMinute events

(><) :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
(><) f g (x, y) = (f x, g y)

minutesByGuard :: [Watch] -> Map Guard Int
minutesByGuard watches = foldl (flip folder) Map.empty subtotals
  where
    subtotal :: (Guard, [State]) -> (Guard, Int)
    subtotal = id >< (length . filter ((==) Asleep))
    subtotals = map subtotal watches
    add :: Int -> Maybe Int -> Maybe Int
    add this (Just prev) = Just $ prev + this
    add this Nothing     = Just this
    folder :: (Guard, Int) -> Map Guard Int -> Map Guard Int
    folder (guard, s) = Map.alter (add s) guard

guardWithMostMinutesAsleep :: [Watch] -> Guard
guardWithMostMinutesAsleep =
  fst . maximumBy (compare `on` snd) . Map.toList . minutesByGuard

_asleepInMinute :: Int -> [[State]] -> Int
_asleepInMinute m = length . filter ((==) Asleep) . map (flip (!!) m)

asleepInMinute :: Guard -> Int -> [Watch] -> Int
asleepInMinute guard m = _asleepInMinute m . map snd . filter ((==) guard . fst)

guardsMostFrequentMinute :: Guard -> [Watch] -> Int
guardsMostFrequentMinute guard ws = ix
  where
    minutes = map (\m -> asleepInMinute guard m ws) [0 .. 59]
    mx = maximum minutes
    ix = fromJust $ elemIndex mx minutes
