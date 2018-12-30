module Solvers.Dec9Spec where

  import Test.Hspec
  import Test.QuickCheck
  import Data.Foldable (toList)

  import Solvers.Dec9

  spec :: Spec
  spec = describe "Dec 9" $ do
    context "marble lists" $ do
      it "can fold into lists" $ property $ \b -> \a ->
        let
          expected = reverse (b :: [Int]) ++ (a :: [Int])
          actual = toList $ ML (b, a)
        in
          actual `shouldBe` expected
      it "rotating left does not change item order" $ property $ \b -> \a ->
        let
          expected = (reverse b ++ a) :: [Int]
          actual = toList . left 1 $ ML (b, a)
        in
          actual `shouldBe` expected
      it "rotating right does not change item order" $ property $ \b -> \a ->
        let
          expected = (reverse b ++ a) :: [Int]
          actual = toList . right 1 $ ML (b, a)
        in
          actual `shouldBe` expected
      it "rotating a non-empty marble list left ensures at least one item in the after list" $ property $ \b -> \a ->
        let
          ml = ML (b, a) :: MarbleList Int
          n = length ml
          ML (_, a') = left 1 ml
        in
          length a' > 0 `shouldBe` n > 0
      it "rotating a non-empty marble list right ensures at least one item in the before list" $ property $ \b -> \a ->
        let
          ml = ML (b, a) :: MarbleList Int
          n = length ml
          ML (b', _) = right 1 ml
        in
          length b' > 0 `shouldBe` n > 0

    context "a" $ do
      it "can parse the input" $ do
        parse "428 players; last marble is worth 70825 points" `shouldBe` (428,70825)

      describe "placing marbles" $ do
        it "works for the initial state" $ do
          let ml0 = empty :: MarbleList Int
          let ml1 = insert 0 ml0

          ml1 `shouldBe` ML ([], [0])

      it "can create the initial state" $ do
        initial (9,25) `shouldBe` State { marbles = ML ([], [0]), toBePlaced = 1, toPlay = 0, scores = [0,0,0,0,0,0,0,0,0], lastMarble = 25 }

      it "can place a regular marble" $ do
        let input = State { marbles = ML ([8,0], [4,2,5,1,6,3,7]), toBePlaced = 9, toPlay = 8, scores = [0,0,0,0,0,0,0,0,0], lastMarble = 25 }
        let expected = State { marbles = ML ([2,4,8,0], [9,5,1,6,3,7]), toBePlaced = 10, toPlay = 0, scores = [0,0,0,0,0,0,0,0,0], lastMarble = 25 }

        place input `shouldBe` expected

      it "can score" $ do
        let input = State {
          marbles = ML ([22,16,8,17,4,18,9,19,2,20,10,21,5], [11,1,12,6,13,3,14,7,15,0]),
          toPlay = 4,
          toBePlaced = 23,
          scores = [0,0,0,0,0,0,0,0,0],
          lastMarble = 25
        }
        let expected = State {
          marbles = ML ([19,2,20,10,21,5], [18,4,17,8,16,22,11,1,12,6,13,3,14,7,15,0]),
          toPlay = 5,
          toBePlaced = 24,
          scores = [0,0,0,0,32,0,0,0,0],
          lastMarble = 25
        }

        score input `shouldBe` expected

      it "can play the full example game" $ do
        let s0 = State { marbles = ML ([], [0]), toBePlaced = 1, toPlay = 0, scores = [0,0,0,0,0,0,0,0,0], lastMarble = 25 }
        let expected = State { marbles = ML ([20,24,2,19,18,4,17,8,16,0],[25,10,21,5,22,11,1,12,6,13,3,14,7,15]), toBePlaced = 26, toPlay = 7, scores = [0,0,0,0,32,0,0,0,0], lastMarble = 25 }

        playGame s0 `shouldBe` expected

      it "can solve the examples" $ do
        solveA ["9 players; last marble is worth 25 points"] `shouldBe` "32"
        solveA ["10 players; last marble is worth 1618 points"] `shouldBe` "8317"
        solveA ["13 players; last marble is worth 7999 points"] `shouldBe` "146373"
        solveA ["17 players; last marble is worth 1104 points"] `shouldBe` "2764"
        solveA ["21 players; last marble is worth 6111 points"] `shouldBe` "54718"
        solveA ["30 players; last marble is worth 5807 points"] `shouldBe` "37305"
