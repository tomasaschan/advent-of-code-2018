module Data.ComputerSpec where

import Test.Hspec
import Data.Computer

import qualified Data.IntMap

spec :: Spec
spec = describe "The computer" $ do
  it "has a bunch of enumerable opcodes" $ do
    enumFrom AddR `shouldBe` [AddR, AddI, MulR, MulI, BanR, BanI, BorR, BorI, SetR, SetI, GtIR, GtRI, GtRR, EqIR, EqRI, EqRR]

  it "the initial registry is all zeros" $ do
    let expected = Registry $ Data.IntMap.fromList [(0,0),(1,0),(2,0),(3,0)]
    initial 4 `shouldBe` expected

  it "can initialize the registry with other values" $ do
    let expected = Registry $ Data.IntMap.fromList [(0,4),(1,2),(2,3),(3,1)]
    Data.Computer.fromList [4,2,3,1] `shouldBe` expected
