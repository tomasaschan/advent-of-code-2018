module Solvers.Dec14Spec where

  import Test.Hspec
  import Data.RandomAccessList

  import Solvers.Dec14

  spec :: Spec
  spec = describe "Dec 14" $ do
    it "has a correct initial state" $ do
      initial `shouldBe` State { recipies = (fromList $ reverse [3,7]), elf1 = 0, elf2 = 1 }

    it "correctly looks up recipies" $ do
      let rs = recipies initial
      recipe 0 rs `shouldBe` 3
      recipe 1 rs `shouldBe` 7

    it "calculates a correct first step" $ do
      next initial `shouldBe` State { recipies = fromList $ reverse [3,7,1,0], elf1 = 0, elf2 = 1 }

    it "calcualtes a correct second step" $ do
      let s0 = State { recipies = fromList $ reverse [3,7,1,0], elf1 = 0, elf2 = 1 }
      let s1 = State { recipies = fromList $ reverse [3,7,1,0,1,0], elf1 = 4, elf2 = 3 }

      next s0 `shouldBe` s1

    it "solves examples for a" $ do
      let samples = [
                      ("0",   "3710101245"),
                      ("1",   "7101012451"),
                      ("2",   "1010124515"),
                      ("3",   "0101245158"),
                      ("4",   "1012451589"),
                      ("5",   "0124515891"),
                      ("6",   "1245158916"),
                      ("7",   "2451589167"),
                      ("8",   "4515891677"),
                      ("9",   "5158916779"),
                      ("10",  "1589167792"),
                      ("18",  "9251071085"),
                      ("2018","5941429882")
                    ]
      solveA (fmap fst samples) `shouldBe` unwords (fmap snd samples)

    -- it "solves for arno's input" $ do
    --   solveA ["503761"] `shouldBe` "1044257397"
