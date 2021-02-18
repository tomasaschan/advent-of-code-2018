module ParseSpec where

import           Data.Map.Strict
import           Debug.Trace
import           ImmuneSystem
import           Test.Hspec

import qualified Parse

spec :: Spec
spec = describe "parsing armies" $ do
    it "parses sample correctly" $ do
        input <- readFile "../../input/dec24-sample.txt"

        let expected =
                Right
                    . Armies
                    . fromList
                    $ [ ( (ImmuneSystem, 1)
                        , Group
                            { count      = 17
                            , hp         = 5390
                            , weaknesses = Weaknesses
                                               ["radiation", "bludgeoning"]
                            , immunities = Immunities []
                            , damage     = 4507
                            , attack     = "fire"
                            , initiative = 2
                            }
                        )
                      , ( (ImmuneSystem, 2)
                        , Group { count      = 989
                                , hp         = 1274
                                , weaknesses = Weaknesses ["bludgeoning", "slashing"]
                                , immunities = Immunities ["fire"]
                                , damage     = 25
                                , attack     = "slashing"
                                , initiative = 3
                                }
                        )
                      , ( (Infection, 1)
                        , Group { count      = 801
                                , hp         = 4706
                                , weaknesses = Weaknesses ["radiation"]
                                , immunities = Immunities []
                                , damage     = 116
                                , attack     = "bludgeoning"
                                , initiative = 1
                                }
                        )
                      , ( (Infection, 2)
                        , Group { count      = 4485
                                , hp         = 2961
                                , weaknesses = Weaknesses ["fire", "cold"]
                                , immunities = Immunities ["radiation"]
                                , damage     = 12
                                , attack     = "slashing"
                                , initiative = 4
                                }
                        )
                      ]
        Parse.armies (traceId input) `shouldBe` expected
