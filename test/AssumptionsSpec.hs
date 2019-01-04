module AssumptionsSpec where

  import Test.Hspec
  import Test.Hspec.QuickCheck
  import Data.Digits
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
    context "in the language" $ do
      it "(<15) 3 returns true" $ do
        let f = (<15)
        let x = 3 :: Int
        f x `shouldBe` True

    context "in other packages" $ do
      describe "digits" $ do
        it "does the reasonable thing for [1..20]" $ do
          let samples = [
                          (1, [1]),
                          (2, [2]),
                          (3, [3]),
                          (4, [4]),
                          (5, [5]),
                          (6, [6]),
                          (7, [7]),
                          (8, [8]),
                          (9, [9]),
                          (10,[1,0]),
                          (11,[1,1]),
                          (12,[1,2]),
                          (13,[1,3]),
                          (14,[1,4]),
                          (15,[1,5]),
                          (16,[1,6]),
                          (17,[1,7]),
                          (18,[1,8]),
                          (19,[1,9]),
                          (20,[2,0])
                        ] :: [(Int,[Int])]
          let expected = fmap snd samples
          let actual = fmap (digits 10 . fst) samples
          actual `shouldBe` expected
        it "yields [] for 0. stupid!" $ do
          let expected = [] :: [Int]
          let actual = digits 10 0
          actual `shouldBe` expected

  itos :: Int -> String
  itos i =
    if i > 0
    then printf "+%i" i
    else show i

  cleanInt :: String -> String
  cleanInt s = [ c | c <- s, not (c == '+')]
