module SolverOld where

import           Debug.Trace

import           Data.Foldable.Extended         ( mapSize )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Text.Printf                    ( printf )

import           Parser                         ( underground )

type Underground = Set (Int, Int)

data Water = Falling
           | Settled
           deriving (Eq, Show)

type Flow = Map (Int, Int) Water

tr :: Int -> Int -> Underground -> Flow -> String -> a -> a
tr x y ug f s = trace (draw (ug, f)) $ trace (printf "(%d,%d) %s" x y s)

pour :: Underground -> (Underground, Flow)
pour ug = (ug, fall 0 ug 500 1 Map.empty)

fall :: Int -> Underground -> Int -> Int -> Flow -> Flow
fall i ug x y f | y > maximum (Set.map snd ug) =
  tr x y ug f (printf "fall 1 %d" i) f
fall i ug x y f | i > 10 = tr x y ug f (printf "fall 6 %d" i) f
fall i ug x y f | onSurface ug x y f && shouldSettle ug x y f =
  tr x y ug f (printf "fall 2 %d" i) settle ug x y f
fall i ug x y f | onSurface ug x y f =
  tr x y ug f (printf "fall 3 %d" i) fall (i + 1) ug x y $ spread ug x y f
fall 0 ug x y f =
  tr x y ug f (printf "fall 4 %d" (0 :: Int)) fall 1 ug x y
    $ fall 0 ug x (y + 1) f
fall i ug x y f =
  tr x y ug f (printf "fall 5 %d" i) Map.insert (x, y) Falling f


data Direction = Pos | Neg

dx :: Direction -> Int
dx Pos = 1
dx Neg = -1

spread :: Underground -> Int -> Int -> Flow -> Flow
spread ug x y = spread' Pos x . spread' Neg x
 where
  spread' d x' f | onSurface ug x' y f && (x' + dx d, y) `elem` ug =
    let f' = Map.insert (x', y) Falling f
    in  tr x' y ug f' (printf "spread 1 %d" $ dx d) f'
  spread' d x' f | onSurface ug x' y f =
    let f' = Map.insert (x', y) Falling $ spread' d (x' + dx d) f
    in  tr x' y ug f' (printf "spread 2 %d" $ dx d) f'
  spread' d x' f =
    let f' = fall 0 ug x' y f in tr x' y ug f' (printf "spread 3 %d" $ dx d) f'


settle :: Underground -> Int -> Int -> Flow -> Flow
settle ug x y = settle' Pos x . settle' Neg x
 where
  settle' d x' f | (x' + dx d, y) `elem` ug = tr x'
                                                 y
                                                 ug
                                                 f'
                                                 (printf "settle 1 %d" $ dx d)
                                                 f'
    where f' = Map.insert (x', y) Settled f
  settle' d x' f = tr x' y ug f' (printf "settle 2 %d" $ dx d) f'
    where f' = Map.insert (x', y) Settled $ settle' d (x' + dx d) f

onSurface :: Underground -> Int -> Int -> Flow -> Bool
onSurface ug x y _ | (x, y + 1) `elem` ug = True
onSurface _ x y f | Map.lookup (x, y + 1) f == Just Settled = True
onSurface _ _ _ _                         = False

walled :: Direction -> Underground -> Int -> Int -> Flow -> Bool
walled d ug x y f | onSurface ug x y f && (x + dx d, y) `elem` ug = True
walled d ug x y f | onSurface ug x y f = walled d ug (x + dx d) y f
walled _ _ _ _ _                       = False

shouldSettle :: Underground -> Int -> Int -> Flow -> Bool
shouldSettle ug x y f | onSurface ug x y f =
  walled Pos ug x y f && walled Neg ug x y f
shouldSettle _ _ _ _ = False

solveA :: [String] -> String
solveA = (++) "\n" . either show (draw . pour) . underground



draw :: (Set (Int, Int), Map (Int, Int) Water) -> String
draw (ug, flow) =
  let ((minx, miny), (maxx, maxy)) = mapSize ug
      xs                           = [minx - 1 .. maxx + 1]
      ys                           = [(min 0 miny) .. maxy]
      square 500 0 = '+'
      square x   y = case Map.lookup (x, y) flow of
        Just Falling               -> '|'
        Just Settled               -> '~'
        Nothing | (x, y) `elem` ug -> '#'
        _                          -> '.'
      line y = printf "%5d " y ++ [ square x y | x <- xs ]
      hundreds x = x `div` 100
      tens x = (x `mod` 100) `div` 10
      ones x = (x `mod` 10)
      row100 = "      " ++ foldMap (show . hundreds) xs
      row10  = "      " ++ foldMap (show . tens) xs
      row1   = "      " ++ foldMap (show . ones) xs
  in  unlines $ [row100, row10, row1] ++ fmap line ys
