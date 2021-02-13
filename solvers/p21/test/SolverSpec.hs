module SolverSpec where

import           Computer
import           Solver

import           Test.Hspec

spec :: Spec
spec = do
  it "can increment the instruction pointer" $ do
    let (_, s) = state (One, [])
    ip s `shouldBe` 0

    let s' = incIp s
    ip s' `shouldBe` 1

  it "terminates" $ do
    let (p, s) =
          state
            ( One
            , [ Op (Set (Immediate 123) Five)
              , Op (Ban Five (Immediate 456) Five)
              ]
            )

    let s' = runUntil (const ((/= 0) . ip)) p s

    ip s' `shouldBe` 1
