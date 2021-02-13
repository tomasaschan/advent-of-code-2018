module Data.List.ExtendedSpec where

import           Test.Hspec

import           Data.Function                  ( on )
import           Data.List.Extended             ( readingOrder
                                                , sortBy
                                                )

spec :: Spec
spec = context "List utilities" $ do
  describe "Reading order sorting" $ do
    it "sorts different-row coordinates with the lowest row number first" $ do
      let a = (1, 2) :: (Int, Int)
      let b = (2, 0) :: (Int, Int)
      readingOrder a b `shouldBe` GT
      sortBy (readingOrder) [a, b] `shouldBe` [b, a]
    it "sorts same-row coordinates with the lowest column number first" $ do
      let a = (2, 0) :: (Int, Int)
      let b = (1, 0) :: (Int, Int)
      readingOrder a b `shouldBe` GT
      sortBy (readingOrder) [a, b] `shouldBe` [b, a]
    it "sorts stably" $ do
      let a = ((1, 2), 'a') :: ((Int, Int), Char)
      let b = ((2, 0), 'b') :: ((Int, Int), Char)
      let c = ((2, 0), 'c') :: ((Int, Int), Char)
      let d = ((1, 2), 'd') :: ((Int, Int), Char)
      (readingOrder `on` fst) a b `shouldBe` GT
      sortBy (readingOrder `on` fst) [a, b, c, d] `shouldBe` [b, c, a, d]
