module SolverSpec where

import           Test.Hspec

import           Solver

spec :: Spec
spec =
  describe "Dec 5" $ do
    context "examples for a" $ do
      it "'aA' -> ''" $ do annihilate "aA" `shouldBe` ""
      it "'abBA' -> ''" $ do annihilate "abBA" `shouldBe` ""
      it "'abAB' -> 'abAB'" $ do annihilate "abAB" `shouldBe` "abAB"
      it "'aabAAB' -> 'aabAAB'" $ do annihilate "aabAAB" `shouldBe` "aabAAB"
      it "'dabAcCaCBAcCcaDA' -> 'dabCBAcaDA'" $ do
        annihilate "dabAcCaCBAcCcaDA" `shouldBe` "dabCBAcaDA"
      it "solves the full example" $ do
        solveA ["dabAcCaCBAcCcaDA"] `shouldBe` "10"
    context "examples for b" $ do
      let input = "dabAcCaCBAcCcaDA"
      describe "reducing polymers" $ do
        it "removes A/a units correctly" $ do
          remove 'a' input `shouldBe` "dbcCCBcCcD"
        it "removes B/b units correctly" $ do
          remove 'b' input `shouldBe` "daAcCaCAcCcaDA"
        it "removes C/c units correctly" $ do
          remove 'c' input `shouldBe` "dabAaBAaDA"
        it "removes D/d units correctly" $ do
          remove 'd' input `shouldBe` "abAcCaCBAcCcaA"
      describe "annihilating reduced polymers" $ do
        let reduceReact c = annihilate . remove c
        it "gets the correct polymer reducing a" $ do
          reduceReact 'a' input `shouldBe` "dbCBcD"
        it "gets the correct polymer reducing b" $ do
          reduceReact 'b' input `shouldBe` "daCAcaDA"
        it "gets the correct polymer reducing c" $ do
          reduceReact 'c' input `shouldBe` "daDA"
        it "gets the correct polymer reducing d" $ do
          reduceReact 'd' input `shouldBe` "abCBAc"
      it "solves the full example" $ do solveB [input] `shouldBe` "4"
