module SolverSpec where

import           Data.Either.Extended
import           Test.Hspec
import           Text.ParserCombinators.Parsec (parse)

import           Solver

spec :: Spec
spec =
  describe "Dec 17" $ do
    context "parsing" $ do
      it "parses a single number" $ do
        parse number "number" "123" `shouldBe` Right 123
      it "parses a range of numbers" $ do
        parse coords "range" "1..23" `shouldBe` Right [1 .. 23]
      describe "parsing x-coordinates" $ do
        it "parses a single coord" $ do parse pXs "x" "x=7" `shouldBe` Right [7]
        it "parses a range" $ do
          parse pXs "xs" "x=7..12" `shouldBe` Right [7 .. 12]
      it "parses a horizontal vein" $ do
        parse horizontal "horiz" "y=1624, x=484..503" `shouldBe`
          Right ([484 .. 503], [1624])
      it "parses a vertical vein" $ do
        parse vertical "vert" "x=426, y=326..342" `shouldBe`
          Right ([426], [326 .. 342])
      it "parses sample input" $ do
        let puzzleInput =
              [ "x=495, y=2..7"
              , "y=7, x=495..501"
              , "x=501, y=3..7"
              , "x=498, y=2..4"
              , "x=506, y=1..2"
              , "x=498, y=10..13"
              , "x=504, y=10..13"
              , "y=13, x=498..504"
              ]
        let parsed = foldAll . fmap (parse vein "vein") $ puzzleInput
        let expected =
              [ ([495], [2 .. 7])
              , ([495 .. 501], [7])
              , ([501], [3 .. 7])
              , ([498], [2 .. 4])
              , ([506], [1 .. 2])
              , ([498], [10 .. 13])
              , ([504], [10 .. 13])
              , ([498 .. 504], [13])
              ]
        parsed `shouldBe` Right expected
