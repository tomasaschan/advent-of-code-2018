module Solvers.Dec8Spec where

  import Test.Hspec

  import Solvers.Dec8

  spec :: Spec
  spec = describe "Dec 8" $ do
    let input = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
    let d = NodeInfo {
      header = Header { childCount = 0, metaCount = 1 },
      children = [],
      meta = [99]
    }
    let c = NodeInfo {
      header = Header { childCount = 1, metaCount = 1 },
      children = [d],
      meta = [2]
    }
    let b = NodeInfo {
      header = Header { childCount = 0, metaCount = 3 },
      children = [],
      meta = [10,11,12]
    }
    let a = NodeInfo {
      header = Header { childCount = 2, metaCount = 3 },
      children = [b, c],
      meta = [1,1,2]
    }

    context "example for a" $ do
      it "can parse the input" $ do
        parse input `shouldBe` a
      it "can correctly sum metadata" $ do
        sumMeta a `shouldBe` 138
      it "solves the example" $ do
        solveA [input] `shouldBe` "138"
    context "examples for b" $ do
      it "node D has a value of 99" $ do
        nodeValue d `shouldBe` 99
      it "node C has a value of 0" $ do
        nodeValue c `shouldBe` 0
      it "node B has a value of 33" $ do
        nodeValue b `shouldBe` 33
      it "node A has a value of 66" $ do
        nodeValue a `shouldBe` 66
      it "solves the example" $ do
        solveB [input] `shouldBe` "66"
