module Solver 
  ( solveA
  , solveB
  )
where

import Data.Foldable.Extended (mapSize)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf (printf)

import Parser (underground)

type Underground = Set (Int, Int)

data Water = Running
           | Settled
           deriving (Eq, Show)

newtype Flow = Flow (Map (Int,Int) Water)

data State = State Underground Flow

solveA :: [String] -> String
solveA = either show (show . countWet . fill) . underground

solveB :: [String] -> String
solveB = either show (show . countSettled . fill) . underground

soak :: Int -> Int -> Water -> State -> State
soak x y w s = s'
  where
    State ug (Flow f) = s
    f' = Flow $ Map.insert (x,y) w f
    s' = State ug f'

isSettled :: Int -> Int -> State -> Bool
isSettled x y (State _ (Flow f)) = Map.lookup (x,y) f == Just Settled
isWet :: Int -> Int -> State -> Bool
isWet x y (State _ (Flow f)) = Map.member (x,y) f
isClay :: Int -> Int -> State -> Bool
isClay x y (State ug _) = (x,y) `elem` ug
isEmpty :: Int -> Int -> State -> Bool
isEmpty x y s = not (isWet x y s) && not (isClay x y s)
isOOB :: Int -> Int -> State -> Bool
isOOB x y (State ug _) = y > maximum (Set.map snd ug)
                      || x < minimum (Set.map fst ug) - 2
                      || x > maximum (Set.map fst ug) + 2

initialState :: Underground -> State
initialState ug = State ug $ Flow Map.empty

type Direction = Int -> Int -> Int
l :: Direction
l = (-)
r :: Direction
r = (+)

fill :: Underground -> State
fill = fillFrom 500 0 . initialState

fillFrom :: Int -> Int -> State -> State
-- fillFrom x y s | isOOB x y s                                                        = s
-- fillFrom x y s | isEmpty x (y+1) s                                                  = fillFrom x (y+1) $ soak x (y+1) Running s
-- fillFrom x y s | supported x y s && not (isWet (x+1) y s) && not (isClay (x+1) y s) = fillFrom (x+1) y $ soak (x+1) y Running s
-- fillFrom x y s | supported x y s && not (isWet (x-1) y s) && not (isClay (x-1) y s) = fillFrom (x-1) y $ soak (x-1) y Running s
-- fillFrom x y s | walled l x y s  && walled r x y s                                  = settle x y s
-- fillFrom _ _ s                                                                      = s
fillFrom x y s | isOOB x y s = s
fillFrom x y s               = settle . spread l . spread r . fall $ s
  where
    fall s' | isEmpty x (y+1) s' = fillFrom x (y+1) $ soak x (y+1) Running s'
    fall s'                      = s'
    
    spread d s' | isSupported x y s' && isEmpty (x `d` 1) y s' = fillFrom (x`d`1) y $ soak (x`d`1) y Running s'
    spread _ s'                                                = s'

    settle s' | not (isSettled x y s') && walled l x y s' && walled r x y s' = fillLevel x y s'
    settle s'                                                                = s'

isSupported :: Int -> Int -> State -> Bool
isSupported x y s | isClay x (y+1) s   = True
isSupported x y (State _ (Flow f)) =
    case Map.lookup (x,y+1) f of
      Just Running -> False
      Just Settled -> True
      Nothing      -> False

fillLevel :: Int -> Int -> State -> State
fillLevel x y = settle' l x . settle' r x
    where
        settle' _ x' s | isOOB x' y s          = s
        settle' d x' s | isClay (x' `d` 1) y s = soak x' y Settled s
        settle' d x' s                         = soak x' y Settled   $ settle' d (x' `d` 1) s

walled :: Direction -> Int -> Int -> State -> Bool
walled d x y s | isClay (x `d` 1) y s  = True
walled d x y s | isEmpty (x `d` 1) y s = False
walled d x y s                         = walled d (x`d` 1) y s

count :: Num b => (Water -> Bool) -> State -> b
count counts (State ug (Flow f)) =
  let ((_, miny), (_, maxy)) = mapSize ug
      agg (_,y) w i = if counts w && miny <= y && y <= maxy then i + 1 else i
  in  Map.foldrWithKey agg 0 f

countSettled :: State -> Int
countSettled = count (==Settled)

countWet :: State -> Int
countWet = count (const True)
  

draw :: State -> String
draw s =
  let (State ug _) = s
      ((minx, miny), (maxx, maxy)) = mapSize ug
      xs = [minx - 3 .. maxx + 3]
      ys = [(min 0 miny) .. maxy]
      square 500 0 = '+'
      square x y | isSettled x y s = '~'
      square x y | isWet x y s     = '|'
      square x y | isClay x y s    = '#'
      square _ _                   = '.'

      line y = printf "%5d " y ++ [square x y | x <- xs]
      hundreds x = x `div` 100
      tens x = (x `mod` 100) `div` 10
      ones x = (x `mod` 10)
      row100 = "      " ++ foldMap (show . hundreds) xs
      row10 = "      " ++ foldMap (show . tens) xs
      row1 = "      " ++ foldMap (show . ones) xs

      drawing = unlines $ [row100, row10, row1] ++ fmap line ys

      results = "Running: " ++ show (count (==Running) s :: Int) ++ "\n"
             ++ "Settled: " ++ show (count (==Settled) s :: Int) ++ "\n"
             ++ "  Total: " ++ show (count (const True) s :: Int)

   in drawing ++ "\n\n" ++ results
