module Assumptions where

  import Test.Hspec
  import Test.Hspec.QuickCheck
  import Text.Printf

  spec :: Spec
  spec = describe "Assumptions" $ do
    context "in the standard library" $ do
      context "my itos helper does what i think it does" $ do
        it "returns +1 for 1" $ do
          (itos 1) `shouldBe` "+1"
        it "returns -1 for -1" $ do
          (itos (negate 1)) `shouldBe` "-1"
        it "returns 0 for 0" $ do
          (itos 0) `shouldBe` "0"
      context "i understand how read works" $ do
        prop "for integers" $
          \i -> (read . cleanInt . itos) i == (i :: Int)

  itos :: Int -> String
  itos i =
    if i > 0
    then printf "+%i" i
    else show i

  cleanInt :: String -> String
  cleanInt s = [ c | c <- s, not (c == '+')]
