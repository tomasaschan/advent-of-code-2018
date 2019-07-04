module SolverSpec where

import           Solver

import           Test.Hspec

spec :: Spec
spec =
  describe "Dec 6" $ do
    let input = ["1, 1", "1, 6", "8, 3", "3, 4", "5, 5", "8, 9"]
    let g = grid $ points input
    let cg = cogrid $ copoints input
    let (x, y) = size (CG cg)
    let s = (x, y)
    context "parsing" $ do it "parses (1,2)" $ do coord "1, 2" `shouldBe` (1, 2)
    context "labelling coords" $ do
      it "gives chords character ids" $ do
        points input `shouldBe`
          [ (((1, 1), ['A']))
          , (((1, 6), ['B']))
          , (((8, 3), ['C']))
          , (((3, 4), ['D']))
          , (((5, 5), ['E']))
          , (((8, 9), ['F']))
          ]
    context "displaying map" $ do
      it "shows a correct map with labelled points" $ do
        show g `shouldBe`
          "..........\n.A........\n..........\n........C.\n...D......\n.....E....\n.B........\n..........\n..........\n........F.\n"
    context "updating a single coord" $ do
      let correct =
            "A.........\n.A........\n..........\n........C.\n...D......\n.....E....\n.B........\n..........\n..........\n........F.\n"
      it "updates an empty coordinate" $ do
        let g' = updateCoord cg 'A' g (0, 0)
        show g' `shouldBe` correct
      it "leaves a coordinate with a closer candidate untouched" $ do
        let g' = updateCoord cg 'A' g (0, 0)
        let g'' = updateCoord cg 'B' g' (0, 0)
        show g'' `shouldBe` correct
      it "updates a coordinate with a farther candidate" $ do
        let g' = updateCoord cg 'B' g (0, 0)
        let g'' = updateCoord cg 'A' g' (0, 0)
        show g'' `shouldBe` correct
      it "updates a coordinate with a draw" $ do
        let correct' =
              "..........\n.A...X....\n..........\n........C.\n...D......\n.....E....\n.B........\n..........\n..........\n........F.\n"
        let g' = updateCoord cg 'E' g (5, 1)
        let g'' = updateCoord cg 'A' g' (5, 1)
        show g'' `shouldBe` correct'
    context "updating for all coords for a single candidate" $ do
      it
        "writes A to all empty positions, but does not overwrite other coordinates" $ do
        let correct =
              "AAAAAAAAAA\nAAAAAAAAAA\nAAAAAAAAAA\nAAAAAAAACA\nAAADAAAAAA\nAAAAAEAAAA\nABAAAAAAAA\nAAAAAAAAAA\nAAAAAAAAAA\nAAAAAAAAFA\n"
        let g' = markClosestTo cg g 'A'
        showGrid s g' `shouldBe` correct
    context "marking the map" $ do
      let g' = markAreas cg g
      it "marks a single pass correctly" $ do
        let correct =
              "AAAAAXCCCC\nAAAAAXCCCC\nAAADDECCCC\nAADDDECCCC\nXXDDDEECCC\nBBXDEEEECC\nBBBXEEEEXX\nBBBXEEEFFF\nBBBXEEFFFF\nBBBXFFFFFF\n"
        showGrid s g' `shouldBe` correct
      it "only adds A to squares closest to A" $ do
        let correct =
              "AAAAA.....\nAAAAA.....\nAAA.......\nAA........\n..........\n..........\n..........\n..........\n..........\n..........\n"
        showClosestTo s 'A' g' `shouldBe` correct
    context "unmarking stuff" $ do
      let g' = markAreas cg g
      it "can unmark A" $ do
        let correct =
              "......CCCC\n......CCCC\n...DDECCCC\n..DDDECCCC\nXXDDDEECCC\nBBXDEEEECC\nBBBXEEEEXX\nBBBXEEEFFF\nBBBXEEFFFF\nBBBXFFFFFF\n"
        let g'' = unmark g' ('A')
        showGrid s g'' `shouldBe` correct
      it "can find edges" $ do infinites g' `shouldBe` ['A', 'C', 'B', 'F']
      it "can remove all infinites" $ do
        let correct =
              "..........\n..........\n...DDE....\n..DDDE....\n..DDDEE...\n...DEEEE..\n....EEEE..\n....EEE...\n....EE....\n..........\n"
        let g'' = removeInfinites g'
        showGrid s g'' `shouldBe` correct
    context "full example for a" $ do
      let g' = markAreas cg g
      it "finds the largets area" $ do
        let g'' = removeInfinites g'
        let largest = largestArea g''
        largest `shouldBe` 'E'
      it "reports area size" $ do sizeOf (g') 'E' `shouldBe` 17
      it "solves A" $ do solveA input `shouldBe` "17"
    context "calculating total distance" $ do
      it "gets the total distance for (4,3) to 30" $ do
        totalDistance cg (4, 3) `shouldBe` 30
      it "correctly marks the area within 32" $ do
        let correct =
              "..........\n..........\n..........\n...XXX....\n..XXXXX...\n..XXXXX...\n...XXX....\n..........\n..........\n..........\n"
        show (markWithin cg (x - 1, y) 32) `shouldBe` correct
    context "full example for b" $ do
      it "solves B" $ do solveB 32 input `shouldBe` "16"
