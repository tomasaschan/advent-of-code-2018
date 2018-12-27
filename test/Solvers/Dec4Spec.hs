module Solvers.Dec4Spec where

  import Test.Hspec
  import Parse

  import Solvers.Dec4

  spec :: Spec
  spec = describe "Dec 4" $ do
    let parse = extract buildEvent . parseRow
    context "parsing" $ do
      it "can parse a wake-up sample" $ do
        let sample = "[1518-11-21 00:43] wakes up"
        parse sample `shouldBe` Event { month = 11, day = 21, hour = 00, minute = 43, what = WakeUp }

      it "can parse a fall-asleep sample" $ do
        let sample = "[1518-11-22 00:04] falls asleep"
        parse sample `shouldBe` Event { month = 11, day = 22, hour = 00, minute = 04, what = FallAsleep }

      it "can parse a begin shift sample" $ do
        let sample = "[1518-11-05 00:01] Guard #3209 begins shift"
        parse sample `shouldBe` Event { month = 11, day = 05, hour = 00, minute = 01, what = BeginShift 3209 }

      it "moves events before midnight to midnight" $ do
        let sample = "[1518-11-12 23:57] Guard #1811 begins shift"
        parse sample `shouldBe` Event { month = 11, day = 13, hour = 00, minute = 00, what = BeginShift 1811 }

    context "building state matrix" $ do
      it "can build one row" $ do
        let events = map parse ["[1518-11-05 00:01] Guard #3209 begins shift", "[1518-11-05 00:04] falls asleep", "[1518-11-05 00:43] wakes up"]
        let row = [Away] ++ replicate 3 Awake ++ replicate 39 Asleep ++ replicate 17 Awake
        buildRow events `shouldBe` (3209, row)

      it "can split event stream into rows" $ do
        let events = map parse ["[1518-11-05 00:01] Guard #3209 begins shift", "[1518-11-05 00:04] falls asleep", "[1518-11-05 00:43] wakes up", "[1518-11-05 00:01] Guard #3209 begins shift", "[1518-11-05 00:04] falls asleep", "[1518-11-05 00:43] wakes up"]
        (length $ buildMatrix events) `shouldBe` 2

    context "can solve the examples" $ do
      let input = lines "[1518-11-01 00:00] Guard #10 begins shift\n[1518-11-01 00:05] falls asleep\n[1518-11-01 00:25] wakes up\n[1518-11-01 00:30] falls asleep\n[1518-11-01 00:55] wakes up\n[1518-11-01 23:58] Guard #99 begins shift\n[1518-11-02 00:40] falls asleep\n[1518-11-02 00:50] wakes up\n[1518-11-03 00:05] Guard #10 begins shift\n[1518-11-03 00:24] falls asleep\n[1518-11-03 00:29] wakes up\n[1518-11-04 00:02] Guard #99 begins shift\n[1518-11-04 00:36] falls asleep\n[1518-11-04 00:46] wakes up\n[1518-11-05 00:03] Guard #99 begins shift\n[1518-11-05 00:45] falls asleep\n[1518-11-05 00:55] wakes up"
      it "for a" $ do
        let answer = solveA input
        answer `shouldBe` "240"
      it "for b" $ do
        let answer = solveB input
        answer `shouldBe` "4455"

