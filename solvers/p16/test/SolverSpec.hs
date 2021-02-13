module SolverSpec where

import           Test.Hspec              hiding ( after
                                                , before
                                                )

import           Data.Computer
import           Solver
import           Text.ParserCombinators.Parsec  ( parse )

spec :: Spec
spec = describe "Dec 16" $ do
  context "parsing" $ do
    it "correctly parses a registry" $ do
      let input    = "[2, 3, 2, 2]"
      let expected = fromList [2, 3, 2, 2]
      parse registry "registry" input `shouldBe` Right expected
    it "correctly parses a masked operation" $ do
      let input    = "15 3 2 2"
      let expected = (15, 3, 2, 2)
      parse maskedOperation "masked op" input `shouldBe` Right expected
    it "correctly parses a full sample" $ do
      let input = "Before: [2, 3, 2, 2]\n15 3 2 2\nAfter:  [2, 3, 4, 2]"
      let expected = Sample { before = fromList [2, 3, 2, 2]
                            , opCode = 15
                            , args   = (3, 2, 2)
                            , after  = fromList [2, 3, 4, 2]
                            }
      parse sample "sample" input `shouldBe` Right expected
  it "counting matches" $ do
    let s = Sample { before = fromList [3, 2, 1, 1]
                   , opCode = 9
                   , args   = (2, 1, 2)
                   , after  = fromList [3, 2, 2, 1]
                   }
    matchCount s `shouldBe` 3
