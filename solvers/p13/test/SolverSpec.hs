module SolverSpec where

import           Test.Hspec

import           Data.Function      (on)
import           Data.List.Extended (readingOrder, sortBy)
import           Data.Map           (fromList, toList)
import           Solver

spec :: Spec
spec =
  context "Dec 13" $ do
    let input =
          [ "/->-\\        "
          , "|   |  /----\\"
          , "| /-+--+-\\  |"
          , "| | |  | v  |"
          , "\\-+-/  \\-+--/"
          , "  \\------/   "
          ]
    context "parsing" $ do
      it "correctly parses the tracks" $ do
        let expected =
              fromList
                [ ((0, 0), ForwardSlash)
                , ((1, 0), Horizontal)
                , ((2, 0), Horizontal)
                , ((3, 0), Horizontal)
                , ((4, 0), Backslash)
                , ((0, 1), Vertical)
                , ((4, 1), Vertical)
                , ((7, 1), ForwardSlash)
                , ((8, 1), Horizontal)
                , ((9, 1), Horizontal)
                , ((10, 1), Horizontal)
                , ((11, 1), Horizontal)
                , ((12, 1), Backslash)
                , ((0, 2), Vertical)
                , ((2, 2), ForwardSlash)
                , ((3, 2), Horizontal)
                , ((4, 2), Intersection)
                , ((5, 2), Horizontal)
                , ((6, 2), Horizontal)
                , ((7, 2), Intersection)
                , ((8, 2), Horizontal)
                , ((9, 2), Backslash)
                , ((12, 2), Vertical)
                , ((0, 3), Vertical)
                , ((2, 3), Vertical)
                , ((4, 3), Vertical)
                , ((7, 3), Vertical)
                , ((9, 3), Vertical)
                , ((12, 3), Vertical)
                , ((0, 4), Backslash)
                , ((1, 4), Horizontal)
                , ((2, 4), Intersection)
                , ((3, 4), Horizontal)
                , ((4, 4), ForwardSlash)
                , ((7, 4), Backslash)
                , ((8, 4), Horizontal)
                , ((9, 4), Intersection)
                , ((10, 4), Horizontal)
                , ((11, 4), Horizontal)
                , ((12, 4), ForwardSlash)
                , ((2, 5), Backslash)
                , ((3, 5), Horizontal)
                , ((4, 5), Horizontal)
                , ((5, 5), Horizontal)
                , ((6, 5), Horizontal)
                , ((7, 5), Horizontal)
                , ((8, 5), Horizontal)
                , ((9, 5), ForwardSlash)
                ]
        let actual = parse input
        fst actual `shouldBe` expected
      it "correctly parses the cars" $ do
        let expected = [((2, 0), East, L), ((9, 3), South, L)]
        let actual = parse input
        snd actual `shouldBe` expected
    context "moving cars" $ do
      it "correctly sorts according to reading order" $ do
        let cars = [((0, 2), East, L), ((2, 0), South, L), ((0, 2), West, L)]
        let sorted = sortBy (readingOrder `on` position) cars
        let expected =
              [((2, 0), South, L), ((0, 2), East, L), ((0, 2), West, L)]
        sorted `shouldBe` expected
      context "and removing crashes" $ do
        it "returns original list if no collided carts exist" $ do
          let cars =
                fromList .
                zip [0 ..] . sortBy (readingOrder `on` position) . snd . parse $
                input
          let cars' = dedupe cars
          cars' `shouldBe` cars
        it "returns list without crashed cars if there are any" $ do
          let carsWithCrashes =
                [ ((1, 4), North, L)
                , ((0, 2), East, L)
                , ((0, 2), West, L)
                , ((2, 0), South, L)
                ]
          let cars =
                fromList . zip [0 ..] . sortBy (readingOrder `on` position) $
                carsWithCrashes
          cars `shouldBe`
            fromList
              [ (0, ((2, 0), South, L))
              , (1, ((0, 2), East, L))
              , (2, ((0, 2), West, L))
              , (3, ((1, 4), North, L))
              ]
          let cars' = dedupe cars
          let expected =
                fromList .
                filter ((/=) 2 . fst) . filter ((/=) 1 . fst) . toList $
                cars
          cars' `shouldBe` expected
    describe "a complete tick" $ do
      let tracks = fst . parse $ input
      context "with no collisions" $ do
        it "moves all cars" $ do
          let cars = [((2, 0), East, L), ((9, 3), South, L)] :: [Car]
          let expected = [((3, 0), East, L), ((9, 4), East, S)] :: [Car]
          let cars' = tickB tracks cars
          cars' `shouldBe` expected
      context "with collisions at end of tick" $ do
        it "moves all cars and removes collided ones" $ do
          let cars =
                [((2, 0), East, L), ((9, 3), South, L), ((10, 4), West, L)] :: [Car]
          let expected = [((3, 0), East, L)] :: [Car]
          let cars' = tickB tracks cars
          cars' `shouldBe` expected
      context "with collisions mid-tick" $ do
        it "moves all cars and removes collided ones" $ do
          let cars =
                [((2, 0), East, L), ((9, 3), South, L), ((9, 4), East, L)] :: [Car]
          let expected = [((3, 0), East, L)] :: [Car]
          let cars' = tickB tracks cars
          cars' `shouldBe` expected
    describe "a complete simulation" $ do
      it "solves the example" $ do
        let input' =
              lines
                "/--<\\  \n^   |  \n| /<+-\\\n| | | v\n\\>+</ |\n  |   ^\n  \\<->/"
        let answer = solveB input'
        answer `shouldBe` "6,4"
