module Dec1Spec where

  import Test.Hspec

  import Dec1

  spec :: Spec
  spec = describe "1 Dec" $ do
    context "examples for a" $ do
      it "+1, -2, +3, +1 => 3" $ do
        solveA ["+1","-2","+3","+1"] `shouldBe` "3"
      it "+1, +1, +1 =>  3" $ do
        solveA ["+1", "+1", "+1"] `shouldBe` "3"
      it "+1, +1, -2 =>  0" $ do
        solveA ["+1", "+1", "-2"] `shouldBe` "0"
      it "-1, -2, -3 => -6" $ do
        solveA ["-1", "-2", "-3"] `shouldBe` "-6"
    context "examples for b" $ do
      it "+1, -2, +3, +1 first reaches 2 twice" $ do
        solveB ["+1","-2","+3","+1"] `shouldBe` "2"
      it "+1, -1 first reaches 0 twice." $ do
        solveB ["+1", "-1"] `shouldBe` "0"
      it "+3, +3, +4, -2, -4 first reaches 10 twice" $ do
        solveB ["+3", "+3", "+4", "-2", "-4"] `shouldBe` "10"
      it "-6, +3, +8, +5, -6 first reaches 5 twice" $ do
        solveB ["-6", "+3", "+8", "+5", "-6"] `shouldBe` "5"
      it "+7, +7, -2, -7, -4 first reaches 14 twice" $ do
        solveB ["+7", "+7", "-2", "-7", "-4"] `shouldBe` "14"
    context "addP" $ do
      it "addP 0 [] [-3..3] returns -6" $ do
        addP 0 [] [-3..3] `shouldBe` -6
