module Data.PathfindingSpec where

import Data.Set (fromList)
import Data.Pathfinding

import Test.Hspec
import Test.Hspec.QuickCheck

import Data.PSQueue (PSQ, Binding((:->)))
import qualified Data.PSQueue as Q

spec :: Spec
spec = describe "Shortest path searching" $ do
  context "reading order priority" $ do
    let swap (x,y) = (y,x)
    let prio path = (length path, fmap swap path) :: (Int, [(Int,Int)])
    prop "different lengths chooses shortest" $ \x1 -> \y1 -> \x2 -> \y2 ->
      prio [(x1,y1)] `compare` prio [(x2,y2),(x1,y1)] == LT
    prop "same-length chooses lowest y" $ \x -> \y1 -> \y2 ->
      prio [(x,y1)] `compare` prio [(x,y2)] == y1 `compare` y2
    prop "same-length and equal y chooses lowest x" $ \x1 -> \x2 -> \y ->
      prio [(x1, y)] `compare` prio [(x2, y)] == x1 `compare` x2
    prop "same-length discriminates on first differing" $ \x1 -> \x2 -> \y1 -> \y2 ->
      prio [(x1,y1),(x2,y2)] `compare` prio [(x1,y1),(x2,y1)] == y2 `compare` y1
