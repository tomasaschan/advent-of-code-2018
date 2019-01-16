module Solvers.Dec15Spec where

import Test.Hspec
import Solvers.Dec15

import Data.Map (fromList)

spec :: Spec
spec = describe "Dec 15" $ do
  context "walkability" $ do
    let dungeon = fromList [
                             ((0,0),False), ((1,0),False),   ((2,0),False),   ((3,0),False),   ((4,0),False),   ((5,0),False),
                             ((0,1),False), ((1,1),True),    ((2,1),True),    ((3,1),True),    ((4,1),False),   ((5,1),False),
                             ((0,2),False), ((1,2),False),   ((2,2),True),    ((3,2),True),    ((4,2),True),    ((5,2),False),
                             ((0,3),False), ((1,3),False),   ((2,3),False),   ((3,3),True),    ((4,3),True),    ((5,3),False),
                             ((0,4),False), ((1,4),True),    ((2,4),True),    ((3,4),False),   ((4,4),True),    ((5,4),False),
                             ((0,5),False), ((1,5),False),   ((2,5),False),   ((3,5),False),   ((4,5),False),   ((5,5),False)
                           ]
    let units = [Unit Elf (1,1), Unit Goblin (4,3)]
    let world = World dungeon units

    it "does not allow walking into walls" $ do
      walkable world (1,2) `shouldBe` False
    it "does not allow walking outside of the map" $ do
      walkable world (6,4) `shouldBe` False
    it "does not allow walking into other units" $ do
      walkable world (4,3) `shouldBe` False
    it "allows walking into unoccupied open spaces" $ do
      walkable world (2,1) `shouldBe` True
