module SolverSpec where

import           Solver
import           Test.Hspec

spec :: Spec
spec =
  describe "Dec 7" $ do
    let input =
          [ "Step C must be finished before step A can begin."
          , "Step C must be finished before step F can begin."
          , "Step A must be finished before step B can begin."
          , "Step A must be finished before step D can begin."
          , "Step B must be finished before step E can begin."
          , "Step D must be finished before step E can begin."
          , "Step F must be finished before step E can begin."
          ]
    it "solves the example for a" $ do solveA input `shouldBe` "CABDFE"
    it "solves the example for b" $ do solveB 2 0 input `shouldBe` "15"
