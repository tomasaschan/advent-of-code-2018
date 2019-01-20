module Solvers.Dec15Spec where

import Test.Hspec
import Solvers.Dec15

import Data.Map (fromList)

spec :: Spec
spec = describe "Dec 15" $ do
  context "parsing" $ do
    let input = [
                  "#######",
                  "#.G.E.#",
                  "#E.G.E#",
                  "#.G.E##",
                  "#######"
                ]
    let units = [
                  Unit Goblin (2,1) 3 200,
                  Unit Elf    (4,1) 3 200,
                  Unit Elf    (1,2) 3 200,
                  Unit Goblin (3,2) 3 200,
                  Unit Elf    (5,2) 3 200,
                  Unit Goblin (2,3) 3 200,
                  Unit Elf    (4,3) 3 200
                ]

    let dungeon = fromList [ ((x,y), wall x y) | x <- xs, y <- ys ]
                    where
                      xs = [0..6]
                      ys = [0..4]
                      wall x y
                        | x == 0 || x == 6 || y == 0 || y == 4 || (x,y) == (5,3) = Wall
                        | otherwise = Cavern

    let realInput = [
                      "################################",
                      "##########..####################",
                      "##########..G###################",
                      "##########..#.....########.#####",
                      "##########........########G#####",
                      "############...#..########.#####",
                      "################....######.#####",
                      "#################..G####...#####",
                      "################...#..#....#####",
                      "################...G..#.....E###",
                      "##############.G..........G....#",
                      "###########.G...G..............#",
                      "###########G..#####..........###",
                      "###########..#######.........###",
                      "##########.G#########........#.#",
                      "#########...#########....G.....#",
                      "#########...#########.........##",
                      "##..........#########.........##",
                      "######....G.#########.....E....#",
                      "##...........#######.......#...#",
                      "#...G.........#####E.......#####",
                      "##....................#..#######",
                      "##.G.................##.########",
                      "##..#GG.............###...#..###",
                      "#G..#..G.G........G.####.#..E###",
                      "#.....#.##...........###.....###",
                      "#######...............###EE..###",
                      "########.....E........###....###",
                      "########..............####..####",
                      "##########....E....#...###.#####",
                      "###########...EE....#.##########",
                      "################################"
                    ]

    it "parses units correctly" $ do
      parseUnits A input `shouldBe` units
    it "parses map correctly" $ do
      parseDungeon input `shouldBe` dungeon

  context "walkability" $ do
    let dungeon = fromList [
                             ((0,0),Wall), ((1,0),Wall),   ((2,0),Wall),   ((3,0),Wall),   ((4,0),Wall),   ((5,0),Wall),
                             ((0,1),Wall), ((1,1),Cavern), ((2,1),Cavern), ((3,1),Cavern), ((4,1),Wall),   ((5,1),Wall),
                             ((0,2),Wall), ((1,2),Wall),   ((2,2),Cavern), ((3,2),Cavern), ((4,2),Cavern), ((5,2),Wall),
                             ((0,3),Wall), ((1,3),Wall),   ((2,3),Wall),   ((3,3),Cavern), ((4,3),Cavern), ((5,3),Wall),
                             ((0,4),Wall), ((1,4),Cavern), ((2,4),Cavern), ((3,4),Wall),   ((4,4),Cavern), ((5,4),Wall),
                             ((0,5),Wall), ((1,5),Wall),   ((2,5),Wall),   ((3,5),Wall),   ((4,5),Wall),   ((5,5),Wall)
                           ]
    let units = [Unit Elf (1,1) 3 200, Unit Goblin (4,3) 3 200]
    let world = World dungeon units

    it "does not allow walking into walls" $ do
      walkable world (1,2) `shouldBe` False
    it "does not allow walking outside of the map" $ do
      walkable world (6,4) `shouldBe` False
    it "does not allow walking into other units" $ do
      walkable world (4,3) `shouldBe` False
    it "allows walking into unoccupied open spaces" $ do
      walkable world (2,1) `shouldBe` True

  it "can sort units according to reading order" $ do
    let unsorted = [
                     Unit Elf    (4,3) 3 200,
                     Unit Elf    (4,1) 3 200,
                     Unit Elf    (1,2) 3 200,
                     Unit Elf    (5,2) 3 200,
                     Unit Goblin (2,1) 3 200,
                     Unit Goblin (3,2) 3 200,
                     Unit Goblin (2,3) 3 200
                   ]
    let sorted = [
                    Unit Goblin (2,1) 3 200,
                    Unit Elf    (4,1) 3 200,
                    Unit Elf    (1,2) 3 200,
                    Unit Goblin (3,2) 3 200,
                    Unit Elf    (5,2) 3 200,
                    Unit Goblin (2,3) 3 200,
                    Unit Elf    (4,3) 3 200
                  ]

    inReadingOrder unsorted `shouldBe` sorted

  describe "examples for a" $ do
    it "full example" $ do
      let input = ["#######","#.G...#","#...EG#","#.#.#G#","#..G#E#","#.....#","#######"]
      let (World dungeon units) = initial A input
      let answer = solveA . play dungeon $ units
      answer `shouldBe` (47, 590, 27730)

    it "first example" $ do
      let input = ["#######","#G..#E#","#E#E.E#","#G.##.#","#...#E#","#...E.#","#######"]
      let (World dungeon units) = initial A input
      let answer = solveA . play dungeon $ units
      answer `shouldBe` (37, 982, 36334)

    it "second example" $ do
      let input = ["#######","#E..EG#","#.#G.E#","#E.##E#","#G..#.#","#..E#.#","#######"]
      let (World dungeon units) = initial A input
      let answer = solveA . play dungeon $ units
      answer `shouldBe` (46, 859, 39514)

    it "third example" $ do
      let input = ["#######","#E.G#.#","#.#G..#","#G.#.G#","#G..#.#","#...E.#","#######"]
      let (World dungeon units) = initial A input
      let answer = solveA . play dungeon $ units
      answer `shouldBe` (35, 793, 27755)

    it "fourth example" $ do
      let input = ["#######","#.E...#","#.#..G#","#.###.#","#E#G#G#","#...#G#","#######"]
      let (World dungeon units) = initial A input
      let answer = solveA . play dungeon $ units
      answer `shouldBe` (54, 536, 28944)

    it "fifth example" $ do
      let input = ["#########","#G......#","#.E.#...#","#..##..G#","#...##..#","#...#...#","#.G...G.#","#.....G.#","#########"]
      let (World dungeon units) = initial A input
      let answer = solveA . play dungeon $ units
      answer `shouldBe` (20, 937, 18740)
