module SolverSpec where

import           Solver
import           Test.Hspec

spec :: Spec
spec = do
    let run_example :: [String] -> Int -> SpecWith ()
        run_example input expected = it "returns expected output" $ do
            a input `shouldBe` show expected

    describe "first example" $ run_example
        [ " 0,0,0,0"
        , " 3,0,0,0"
        , " 0,3,0,0"
        , " 0,0,3,0"
        , " 0,0,0,3"
        , " 0,0,0,6"
        , " 9,0,0,0"
        , "12,0,0,0"
        ]
        2

    describe "second example" $ run_example
        [ "-1,2,2,0"
        , "0,0,2,-2"
        , "0,0,0,-2"
        , "-1,2,0,0"
        , "-2,-2,-2,2"
        , "3,0,2,-1"
        , "-1,3,2,2"
        , "-1,0,-1,0"
        , "0,2,1,-2"
        , "3,0,0,0"
        ]
        4

    describe "third example" $ run_example
        [ "1,-1,0,1"
        , "2,0,-1,0"
        , "3,2,-1,0"
        , "0,0,3,1"
        , "0,0,-1,-1"
        , "2,3,-2,0"
        , "-2,2,0,0"
        , "2,-2,0,-1"
        , "1,-1,0,-1"
        , "3,2,0,2"
        ]
        3

    describe "fourth example" $ run_example
        [ "1,-1,-1,-2"
        , "-2,-2,0,1"
        , "0,2,1,3"
        , "-2,3,-2,1"
        , "0,2,3,-2"
        , "-1,-1,1,-2"
        , "0,-2,-1,0"
        , "-2,2,3,-1"
        , "1,2,2,0"
        , "-1,-2,0,-2"
        ]
        8
