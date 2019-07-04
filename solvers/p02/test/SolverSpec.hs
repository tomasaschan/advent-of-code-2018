module SolverSpec where

import           Data.List
import           Test.Hspec

import           Solver

spec :: Spec
spec =
  describe "2 Dec" $ do
    context "examples for a" $ do
      it "abcdef contains no letters that appear exactly two or three times." $ do
        hasMultiple 2 "abcdef" `shouldBe` False
        hasMultiple 3 "abcdef" `shouldBe` False
      it "bababc contains two a and three b, so it counts for both." $ do
        hasMultiple 2 "bababc" `shouldBe` True
        hasMultiple 3 "bababc" `shouldBe` True
      it "abbcde contains two b, but no letter appears exactly three times." $ do
        hasMultiple 2 "abbcde" `shouldBe` True
        hasMultiple 3 "abbcde" `shouldBe` False
      it "abcccd contains three c, but no letter appears exactly two times." $ do
        hasMultiple 2 "abcccd" `shouldBe` False
        hasMultiple 3 "abcccd" `shouldBe` True
      it "aabcdd contains two a and two d, but it only counts once." $ do
        hasMultiple 2 "aabcdd" `shouldBe` True
        hasMultiple 3 "aabcdd" `shouldBe` False
      it "abcdee contains two e." $ do
        hasMultiple 2 "abcdee" `shouldBe` True
        hasMultiple 3 "abcdee" `shouldBe` False
      it "ababab contains three a and three b, but it only counts once." $ do
        hasMultiple 2 "ababab" `shouldBe` False
        hasMultiple 3 "ababab" `shouldBe` True
      it "all of the above has the checksum 12." $ do
        let ids =
              [ "abcdef"
              , "bababc"
              , "abbcde"
              , "abcccd"
              , "aabcdd"
              , "abcdee"
              , "ababab"
              ]
         in checksum ids `shouldBe` 12
    context "examples for b" $ do
      it "abcde and axcye are not 1 letter apart" $ do
        adjacent ("abcde", "axcye") `shouldBe` False
      it "fghij and fguij are 1 letter apart" $ do
        adjacent ("fghij", "fguij") `shouldBe` True
      it "fguij and fuhij are not 1 letter apart" $ do
        adjacent ("fguij", "fuhij") `shouldBe` False
      it "finds the correct id in list of ids" $ do
        (findAdjacent . sort)
          ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"] `shouldBe`
          Just ("fghij", "fguij")
      it "finds the correct common letters in list of ids" $ do
        solveB ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"] `shouldBe`
          "fgij"
