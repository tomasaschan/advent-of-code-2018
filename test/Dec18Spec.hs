module Dec18Spec where

  import Test.Hspec
  import Dec18

  spec :: Spec
  spec = context "Dec 18" $ do
    context "example for a" $ do
      let input =
                  [
                    ".#.#...|#.",
                    ".....#|##|",
                    ".|..|...#.",
                    "..|#.....#",
                    "#.#|||#|#|",
                    "...#.||...",
                    ".|....|...",
                    "||...#|.#|",
                    "|.||||..|.",
                    "...#.|..|."
                  ]
      let initial = createMap $ map parseRow input

      it "correctly identifies surroundings of edge pieces" $ do
        surroundings initial (8,1) `shouldBe` [Lumbermill, Lumbermill, Open, Open, Open, Open, Trees, Lumbermill]

      it "can step once" $ do
        let correct = [
                        ".......##.",
                        "......|###",
                        ".|..|...#.",
                        "..|#||...#",
                        "..##||.|#|",
                        "...#||||..",
                        "||...|||..",
                        "|||||.||.|",
                        "||||||||||",
                        "....||..|."
                      ]
        let stepped = step initial

        displayMap 10 stepped `shouldBe` correct

