module SolverSpec where

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Test.Hspec

import           Solver

spec :: Spec
spec = context "Dec 12" $ do
  context "parsing input" $ do
    it "parses example initial state" $ do
      let input    = "initial state: #..#.#..##......###...###"
      let expected = Set.fromList [0, 3, 5, 8, 9, 16, 17, 18, 22, 23, 24]
      parseInitial input `shouldBe` expected
    it "parses the example rules" $ do
      let input =
            [ "...## => #"
            , "..#.. => #"
            , ".#... => #"
            , ".#.#. => #"
            , ".#.## => #"
            , ".##.. => #"
            , ".#### => #"
            , "#.#.# => #"
            , "#.### => #"
            , "##.#. => #"
            , "##.## => #"
            , "###.. => #"
            , "###.# => #"
            , "####. => #"
            ]
      let expected = Map.fromList
            [ ("...##", '#')
            , ("..#..", '#')
            , (".#...", '#')
            , (".#.#.", '#')
            , (".#.##", '#')
            , (".##..", '#')
            , (".####", '#')
            , ("#.#.#", '#')
            , ("#.###", '#')
            , ("##.#.", '#')
            , ("##.##", '#')
            , ("###..", '#')
            , ("###.#", '#')
            , ("####.", '#')
            ]
      parseRules input `shouldBe` expected
  context "example for a" $ do
    it "solves the example" $ do
      let
        input =
          lines
            "initial state: #..#.#..##......###...###\n\n...## => #\n..#.. => #\n.#... => #\n.#.#. => #\n.#.## => #\n.##.. => #\n.#### => #\n#.#.# => #\n#.### => #\n##.#. => #\n##.## => #\n###.. => #\n###.# => #\n####. => #"
      let answer = "325"
      solveA input `shouldBe` answer
