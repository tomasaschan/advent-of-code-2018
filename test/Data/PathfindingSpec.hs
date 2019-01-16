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
    prop "same-length chooses lowest y" $ \x1 -> \x2 ->
      prio [(x1,1)] `compare` prio [(x2,3)] == LT
    prop "same-length and equal y chooses lowest x" $ \y ->
      prio [(1, y)] `compare` prio [(3, y)] == LT

  context "without obstacles" $ do
    let walkable (x,y) = abs(x) < 10 && abs(y) < 10
    let nexts c = let add (x,y) (x',y') = (x+x',y+y') :: (Int,Int)
                  in fmap (add c) [(0,1),(-1,0),(1,0),(0,-1)] :: [(Int,Int)]

    context "with single target right next to source" $ do
      let source = (2,3)
      let targets = [(3,3)]
      it "finds a path with both coordinates" $ do
        shortestPathReadingOrder walkable nexts source targets `shouldBe` (Just [(2,3), (3,3)])
    context "with a few squares in between" $ do
      let source = (2,3)
      let targets = [(4,6)]
      it "finds the shortest path in reading order" $ do
        shortestPathReadingOrder walkable nexts source targets `shouldBe` (Just [(2,3), (3,3), (4,3), (4,4), (4,5), (4,6)])
