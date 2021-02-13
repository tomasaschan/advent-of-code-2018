module SolverSpec where

import           Solver
import           Test.Hspec

import           Data.Map                       ( fromList )
import           GHC.Exts                       ( the )

spec :: Spec
spec = describe "Dec 15" $ do
  context "parsing" $ do
    let input = ["#######", "#.G.E.#", "#E.G.E#", "#.G.E##", "#######"]
    let units =
          [ Unit Goblin (2, 1) 3 200
          , Unit Elf    (4, 1) 3 200
          , Unit Elf    (1, 2) 3 200
          , Unit Goblin (3, 2) 3 200
          , Unit Elf    (5, 2) 3 200
          , Unit Goblin (2, 3) 3 200
          , Unit Elf    (4, 3) 3 200
          ]
    let
      dungeon = fromList [ ((x, y), wall x y) | x <- xs, y <- ys ]
       where
        xs = [0 .. 6]
        ys = [0 .. 4]
        wall x y
          | x == 0 || x == 6 || y == 0 || y == 4 || (x, y) == (5, 3) = Wall
          | otherwise = Cavern
    it "parses units correctly" $ do
      parseUnits 3 input `shouldBe` units
    it "parses map correctly" $ do
      parseDungeon input `shouldBe` dungeon
  context "walkability" $ do
    let dungeon = fromList
          [ ((0, 0), Wall)
          , ((1, 0), Wall)
          , ((2, 0), Wall)
          , ((3, 0), Wall)
          , ((4, 0), Wall)
          , ((5, 0), Wall)
          , ((0, 1), Wall)
          , ((1, 1), Cavern)
          , ((2, 1), Cavern)
          , ((3, 1), Cavern)
          , ((4, 1), Wall)
          , ((5, 1), Wall)
          , ((0, 2), Wall)
          , ((1, 2), Wall)
          , ((2, 2), Cavern)
          , ((3, 2), Cavern)
          , ((4, 2), Cavern)
          , ((5, 2), Wall)
          , ((0, 3), Wall)
          , ((1, 3), Wall)
          , ((2, 3), Wall)
          , ((3, 3), Cavern)
          , ((4, 3), Cavern)
          , ((5, 3), Wall)
          , ((0, 4), Wall)
          , ((1, 4), Cavern)
          , ((2, 4), Cavern)
          , ((3, 4), Wall)
          , ((4, 4), Cavern)
          , ((5, 4), Wall)
          , ((0, 5), Wall)
          , ((1, 5), Wall)
          , ((2, 5), Wall)
          , ((3, 5), Wall)
          , ((4, 5), Wall)
          , ((5, 5), Wall)
          ]
    let units = [Unit Elf (1, 1) 3 200, Unit Goblin (4, 3) 3 200]
    it "does not allow walking into walls" $ do
      walkable dungeon units (1, 2) `shouldBe` False
    it "does not allow walking outside of the map" $ do
      walkable dungeon units (6, 4) `shouldBe` False
    it "does not allow walking into other units" $ do
      walkable dungeon units (4, 3) `shouldBe` False
    it "allows walking into unoccupied open spaces" $ do
      walkable dungeon units (2, 1) `shouldBe` True
  it "can sort units according to reading order" $ do
    let unsorted =
          [ Unit Elf    (4, 3) 3 200
          , Unit Elf    (4, 1) 3 200
          , Unit Elf    (1, 2) 3 200
          , Unit Elf    (5, 2) 3 200
          , Unit Goblin (2, 1) 3 200
          , Unit Goblin (3, 2) 3 200
          , Unit Goblin (2, 3) 3 200
          ]
    let sorted =
          [ Unit Goblin (2, 1) 3 200
          , Unit Elf    (4, 1) 3 200
          , Unit Elf    (1, 2) 3 200
          , Unit Goblin (3, 2) 3 200
          , Unit Elf    (5, 2) 3 200
          , Unit Goblin (2, 3) 3 200
          , Unit Elf    (4, 3) 3 200
          ]
    inReadingOrder unsorted `shouldBe` sorted
  describe "examples for a" $ do
    it "full example" $ do
      let input =
            [ "#######"
            , "#.G...#"
            , "#...EG#"
            , "#.#.#G#"
            , "#..G#E#"
            , "#.....#"
            , "#######"
            ]
      let (dungeon, units) = initial 3 input
      let answer           = summarize . play dungeon $ units
      answer `shouldBe` (47, 590, 27730, 0)
    it "first example" $ do
      let input =
            [ "#######"
            , "#G..#E#"
            , "#E#E.E#"
            , "#G.##.#"
            , "#...#E#"
            , "#...E.#"
            , "#######"
            ]
      let (dungeon, units) = initial 3 input
      let answer           = summarize . play dungeon $ units
      answer `shouldBe` (37, 982, 36334, 5)
    it "second example" $ do
      let input =
            [ "#######"
            , "#E..EG#"
            , "#.#G.E#"
            , "#E.##E#"
            , "#G..#.#"
            , "#..E#.#"
            , "#######"
            ]
      let (dungeon, units) = initial 3 input
      let answer           = summarize . play dungeon $ units
      answer `shouldBe` (46, 859, 39514, 5)
    it "third example" $ do
      let input =
            [ "#######"
            , "#E.G#.#"
            , "#.#G..#"
            , "#G.#.G#"
            , "#G..#.#"
            , "#...E.#"
            , "#######"
            ]
      let (dungeon, units) = initial 3 input
      let answer           = summarize . play dungeon $ units
      answer `shouldBe` (35, 793, 27755, 0)
    it "fourth example" $ do
      let input =
            [ "#######"
            , "#.E...#"
            , "#.#..G#"
            , "#.###.#"
            , "#E#G#G#"
            , "#...#G#"
            , "#######"
            ]
      let (dungeon, units) = initial 3 input
      let answer           = summarize . play dungeon $ units
      answer `shouldBe` (54, 536, 28944, 0)
    it "fifth example" $ do
      let input =
            [ "#########"
            , "#G......#"
            , "#.E.#...#"
            , "#..##..G#"
            , "#...##..#"
            , "#...#...#"
            , "#.G...G.#"
            , "#.....G.#"
            , "#########"
            ]
      let (dungeon, units) = initial 3 input
      let answer           = summarize . play dungeon $ units
      answer `shouldBe` (20, 937, 18740, 0)
  context "pathfinding" $ do
    it "chooses the correct target square" $ do
      let world = ["#######", "#E..G.#", "#...#.#", "#.G.#G#", "#######"]
      let (dungeon, units) = initial 3 world
      let targets =
            foldMap (inRangeOf . position) . filter ((==) Goblin . race) $ units
      let (Just path) = shortestPathReadingOrder dungeon units (1, 1) targets
      last path `shouldBe` (3, 1)
    it "chooses the correct square to move to" $ do
      let (dungeon, _) =
            initial 3 ["#######", "#.....#", "#.....#", "#.....#", "#######"]
      let mover   = Unit Elf (2, 1) 0 0
      let rest    = [Unit Goblin (4, 3) 0 0]
      let targets = inRangeOf (4, 3)
      let moved   = move dungeon rest targets mover
      position moved `shouldBe` (3, 1)
    it "finds cadaker's subtle bug" $ do
      let (dungeon, units) = initial
            3
            [ "#######"
            , "#..E..#"
            , "#.###.#"
            , "#.#...#"
            , "#...#.#"
            , "###G###"
            , "#######"
            ]
      let mover   = the . filter ((==) Elf . race) $ units
      let target  = the . filter ((==) Goblin . race) $ units
      let targets = inRangeOf . position $ target
      let moved   = move dungeon units targets mover
      position moved `shouldBe` (2, 1)
    it "chooses targets in reading order" $ do
      let (dungeon, units) = initial
            3
            [ "######"
            , "#E...#"
            , "#.##.#"
            , "#..#.#"
            , "##.#.#"
            , "#..#.#"
            , "#.##.#"
            , "#..G.#"
            , "######"
            ]
      let mover   = the . filter ((==) Elf . race) $ units
      let target  = the . filter ((==) Goblin . race) $ units
      let targets = inRangeOf . position $ target
      let moved   = move dungeon units targets mover
      position moved `shouldBe` (1, 2)
