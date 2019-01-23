module Solvers.Dec16Spec where

import Test.Hspec hiding (before,after)

import Data.Computer
import Solvers.Dec16

spec :: Spec
spec = describe "Dec 16" $ do
  context "parsing" $ do
    it "correctly parses a registry" $ do
      let input = "[2, 3, 2, 2]"
      let expected = fromList [2,3,2,2]

      parseMaybe registry input `shouldBe` Just expected

    it "correctly parses a masked operation" $ do
      let input = "15 3 2 2"
      let expected = (15, 3, 2, 2)

      parseMaybe maskedOperation input `shouldBe` Just expected

    it "correctly parses a full sample" $ do
      let input = "Before: [2, 3, 2, 2]\n15 3 2 2\nAfter:  [2, 3, 4, 2]"
      let expected = Sample { before = fromList [2,3,2,2], operation = (15,3,2,2), after = fromList [2,3,4,2] }

      parseMaybe sample input `shouldBe` Just expected
