module DomainSpec where

import           Test.Hspec

import           Domain

spec :: Spec
spec = do
  describe "touches" $ do
    describe "12,12,12" $ do
      let sq = createCube [] 1 (Position 12 12 12)

      describe "straight lines" $ do

        it "11,12,12:1" $ do
          let bot = Nanobot (Position 11 12 12) 1
          touches sq bot `shouldBe` True

        it "10,12,12:2" $ do
          let bot = Nanobot (Position 10 12 12) 2
          touches sq bot `shouldBe` True
