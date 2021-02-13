module SolverSpec where

import           Test.Hspec

import           Solver

import           Data.Map                       ( (!) )

spec :: Spec
spec = describe "Dec 11" $ do
  describe "correctly calculates one cell's power level" $ do
    it "example: 8, (3,5) => 4" $ do
      let coord        = (3, 5)
      let serialNumber = 8
      power serialNumber coord `shouldBe` 4
    it "example: 57, (122,79) => -5" $ do
      let coord        = (122, 79)
      let serialNumber = 57
      power serialNumber coord `shouldBe` -5
    it "example: 39, (217,196) => 0" $ do
      let coord        = (217, 196)
      let serialNumber = 39
      power serialNumber coord `shouldBe` 0
    it "example: 71, (101,153) => 4" $ do
      let coord        = (101, 153)
      let serialNumber = 71
      power serialNumber coord `shouldBe` 4
  it "correctly builds summed area table" $ do
    let magicSquare =
          [ [31, 2, 4, 33, 5, 36]
          , [12, 26, 9, 10, 29, 25]
          , [13, 17, 21, 22, 20, 18]
          , [24, 23, 15, 16, 14, 19]
          , [30, 8, 28, 27, 11, 7]
          , [1, 35, 34, 3, 32, 6]
          ]
    let summed =
          [ [31, 33, 37, 70, 75, 111]
          , [43, 71, 84, 127, 161, 222]
          , [56, 101, 135, 200, 254, 333]
          , [80, 148, 197, 278, 346, 444]
          , [110, 186, 263, 371, 450, 555]
          , [111, 222, 333, 444, 555, 666]
          ]
    let p (x, y) = if x < 1 || y < 1 then 0 else row !! (x - 1)
          where row = magicSquare !! (y - 1)
    let listify sat = [ [ sat ! (x, y) | x <- [1 .. 6] ] | y <- [1 .. 6] ]
    (listify $ summedAreaTable 6 p) `shouldBe` summed
  describe "correctly calculates one super-cell's power level" $ do
    let table sn = summedAreaTable 300 (power sn)
    let solveFor sn x y = areaPowerLookup (table sn) (x, y, 3)
    it "example with serial number 18" $ do
      solveFor 18 33 45 `shouldBe` 29
    it "example with serial number 42" $ do
      solveFor 42 21 61 `shouldBe` 30
    -- describe "solves the example for a" $ do
    --   it "solves the example for serial number 18" $ do
    --     solveA ["18"] `shouldBe` "(33,45)"
    --   it "solves the example for serial number 42" $ do
    --     solveA ["42"] `shouldBe` "(21,61)"
    -- describe "solves the examples for b" $ do
    --   it "solves the example for serial number 18" $ do
    --     solveB ["18"] `shouldBe` "(90,269,16)"
    --   it "solves the example for serial number 42" $ do
    --     solveB ["42"] `shouldBe` "(232,251,12)"
