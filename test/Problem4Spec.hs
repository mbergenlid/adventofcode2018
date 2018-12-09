module Problem4Spec (spec) where

import Test.Hspec
import Problem4
import Data.Time.Clock
import Data.Time.Format
import qualified Data.Map as M

spec :: Spec
spec = do
    describe "A Guard" $ do
        it "Can be empty" $
            totalMinutesAsleep (empty 1) `shouldBe` 0
        it "Can sleep for x minutes" $
            totalMinutesAsleep (sleep (empty 1) (dateTime "1518-11-01 00:05") (dateTime "1518-11-01 00:25")) `shouldBe` 20

    describe "A sorted list of guard shifts" $ do
        it "Minutes 1" $
            fmap totalMinutesAsleep (M.lookup 10 allGuards) `shouldBe` Just 50
        it "Minutes 3" $
            fmap totalMinutesAsleep (M.lookup 99 allGuards) `shouldBe` Just 30

        it "Should count the minutes most likely asleep" $
            fmap (fst . bestMinute) (M.lookup 10 allGuards) `shouldBe` Just 24
        it "Should count the minutes most likely asleep" $
            fmap (fst . bestMinute) (M.lookup 99 allGuards) `shouldBe` Just 45

    describe "Find the guard most frequently asleep in any minute" $
        it "Test 1" $
            guardId (mostFrequentlyAsleep sortedEntries) `shouldBe` 99



allGuards = guards unSortedEntries

sortedEntries = [
    Entry (dateTime "1518-11-01 00:00") (BeginShift 10),
    Entry (dateTime "1518-11-01 00:05") FallAsleep,
    Entry (dateTime "1518-11-01 00:25") WakeUp,
    Entry (dateTime "1518-11-01 00:30") FallAsleep,
    Entry (dateTime "1518-11-01 00:55") WakeUp,
    Entry (dateTime "1518-11-01 23:58") (BeginShift 99),
    Entry (dateTime "1518-11-02 00:40") FallAsleep,
    Entry (dateTime "1518-11-02 00:50") WakeUp,
    Entry (dateTime "1518-11-03 00:05") (BeginShift 10),
    Entry (dateTime "1518-11-03 00:24") FallAsleep,
    Entry (dateTime "1518-11-03 00:29") WakeUp,
    Entry (dateTime "1518-11-04 00:02") (BeginShift 99),
    Entry (dateTime "1518-11-04 00:36") FallAsleep,
    Entry (dateTime "1518-11-04 00:46") WakeUp,
    Entry (dateTime "1518-11-05 00:03") (BeginShift 99),
    Entry (dateTime "1518-11-05 00:45") FallAsleep,
    Entry (dateTime "1518-11-05 00:55") WakeUp]

unSortedEntries = [
    Entry (dateTime "1518-11-01 00:00") (BeginShift 10),
    Entry (dateTime "1518-11-04 00:46") WakeUp,
    Entry (dateTime "1518-11-01 00:55") WakeUp,
    Entry (dateTime "1518-11-01 00:05") FallAsleep,
    Entry (dateTime "1518-11-01 00:25") WakeUp,
    Entry (dateTime "1518-11-01 00:30") FallAsleep,
    Entry (dateTime "1518-11-01 23:58") (BeginShift 99),
    Entry (dateTime "1518-11-02 00:40") FallAsleep,
    Entry (dateTime "1518-11-05 00:03") (BeginShift 99),
    Entry (dateTime "1518-11-02 00:50") WakeUp,
    Entry (dateTime "1518-11-03 00:05") (BeginShift 10),
    Entry (dateTime "1518-11-03 00:24") FallAsleep,
    Entry (dateTime "1518-11-03 00:29") WakeUp,
    Entry (dateTime "1518-11-04 00:02") (BeginShift 99),
    Entry (dateTime "1518-11-04 00:36") FallAsleep,
    Entry (dateTime "1518-11-05 00:45") FallAsleep,
    Entry (dateTime "1518-11-05 00:55") WakeUp]

dateTime :: String -> UTCTime
dateTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M"
