module Dec3Spec where

  import Test.Hspec
  import Data.Map
  import Data.Maybe

  import Dec3

  spec :: Spec
  spec = describe "3 Dec" $ do
    context "parsing #123 @ 3,2: 5x4" $ do
      let input = "#123 @ 3,2: 5x4"
      let parsed = fromJust $ parse input
      let correct = Claim { id' = "123", left = 3, top = 2, width = 5, height = 4 }
      it "id matches" $ do
        id' parsed `shouldBe` id' correct
      it "left matches" $ do
        left parsed `shouldBe` left correct
      it "top matches" $ do
        top parsed `shouldBe` top correct
      it "width matches" $ do
        width parsed `shouldBe` width correct
      it "height matches" $ do
        height parsed `shouldBe` height correct
    context "updating claims map" $ do
      context "correctly inserts a 1" $ do
        it "in an empty fabric" $ do
          let fabric = empty
          claim1 fabric (2,1) `shouldBe` fromList [((2,1), 1)]
        it "in a non-empty fabric with nothing matching the index" $ do
          let fabric = fromList [((2,1), 1)]
          claim1 fabric (1,2) `shouldBe` fromList [((1,2), 1), ((2,1), 1)]
      it "correctly increases by 1" $ do
        let fabric = fromList [((2,1), 1)]
        claim1 fabric (2,1) `shouldBe` fromList [((2,1),2)]
    context "interpreting claims" $ do
      it "yields a correct list of indices for a claim" $ do
        squares Claim { id' = "", left = 3, top = 2, width = 5, height = 4 } `shouldBe` [
          (3,2), (4,2), (5,2), (6,2), (7,2),
          (3,3), (4,3), (5,3), (6,3), (7,3),
          (3,4), (4,4), (5,4), (6,4), (7,4),
          (3,5), (4,5), (5,5), (6,5), (7,5)]
      it "yields a correct fabric for a claim" $ do
        fabric Claim { id' = "", left = 3, top = 2, width = 5, height = 4 } `shouldBe` fromList [
          ((3,2), 1), ((4,2), 1), ((5,2), 1), ((6,2), 1), ((7,2), 1),
          ((3,3), 1), ((4,3), 1), ((5,3), 1), ((6,3), 1), ((7,3), 1),
          ((3,4), 1), ((4,4), 1), ((5,4), 1), ((6,4), 1), ((7,4), 1),
          ((3,5), 1), ((4,5), 1), ((5,5), 1), ((6,5), 1), ((7,5), 1)]
    context "adding claims" $ do
      it "results in a correct map" $ do
        let
          create = fabric . fromJust . parse
          c1 = create "#1 @ 1,3: 4x4"
          c2 = create "#2 @ 3,1: 4x4"
          c3 = create "#3 @ 5,5: 2x2"

          total = combine [c1, c2, c3]
          correct = fromList [
                                    ((1,3),1), ((1,4),1), ((1,5),1), ((1,6),1),
                                    ((2,3),1), ((2,4),1), ((2,5),1), ((2,6),1),
              ((3,1),1), ((3,2),1), ((3,3),2), ((3,4),2), ((3,5),1), ((3,6),1),
              ((4,1),1), ((4,2),1), ((4,3),2), ((4,4),2), ((4,5),1), ((4,6),1),
              ((5,1),1), ((5,2),1), ((5,3),1), ((5,4),1), ((5,5),1), ((5,6),1),
              ((6,1),1), ((6,2),1), ((6,3),1), ((6,4),1), ((6,5),1), ((6,6),1)
            ]
          in
            (display total (8,8)) `shouldBe` (display correct (8,8))