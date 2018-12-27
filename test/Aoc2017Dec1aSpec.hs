module Aoc2017Dec1aSpec where

  import Test.Hspec

  import Aoc2017.Dec1

  spec :: Spec
  spec = describe "2017: dec 1" $ do
    context "a: shifting 1" $ do
      let sut = sumString One
      it "1122 produces 3" $ do
        sut "1122" `shouldBe` 3
      it "1111 produces 4" $ do
        sut "1111" `shouldBe` 4
      it "1234 produces 0" $ do
        sut "1234" `shouldBe` 0
      it "91212129 produces 9" $ do
        sut "91212129" `shouldBe` 9

    context "b: shifting half" $ do
      let sut = sumString Half
      it "1212 produces 6" $ do
        sut "1212" `shouldBe` 6
      it "1221 produces 0" $ do
        sut "1221" `shouldBe` 0
      it "123425 produces 4" $ do
        sut "123425" `shouldBe` 4
      it "123123 produces 1" $ do
        sut "123123" `shouldBe` 12
      it "12131415 produces 4" $ do
        sut "12131415" `shouldBe` 4
