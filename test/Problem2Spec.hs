module Problem2Spec (spec) where

import Test.Hspec
import Problem2
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "Count letter occurence" $ do
        it "should count occurance 1" $
            countLetters "abcdef" `shouldBe` Map.fromList [('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1), ('f', 1)]

        it "should count occurance 2" $
            countLetters "bababc" `shouldBe` Map.fromList [('a', 2), ('b', 3), ('c', 1)]

        it "should count occurance 3" $
            countLetters "abbcde" `shouldBe` Map.fromList [('a', 1), ('b', 2), ('c', 1), ('d', 1), ('e', 1)]

    describe "Value of box ID" $ do
        it "should count no three or twos as one" $
            boxValue "abcdef" `shouldBe` (0,0)

        it "should count" $
            boxValue "bababc" `shouldBe` (1,1)

        it "1 twos and no threes" $
            boxValue "abbcde" `shouldBe` (1,0)

        it "1 threes and no twos" $
            boxValue "abcccd" `shouldBe` (0,1)

        it "2 twos" $
            boxValue "aabcdd" `shouldBe` (1,0)

        it "2 trees" $
            boxValue "ababab" `shouldBe` (0,1)

    describe "Checksum" $ do
        it "should calculate based on the box values" $
            checksum ["abcdef", "bababc"] `shouldBe` 1*1

    describe "Diff" $ do
        it "Should work" $
            diff "axcye" "abcde" `shouldBe` 2

        it "fghij vs fguij should diff by 1" $
            diff "fghij" "fguij" `shouldBe` 1

    describe "Find the IDs that diff by one" $ do
        it "should also work" $
            findOffByPairs ["abcde","fghij","klmno","pqrst","fguij","axcye","wvxyz"] `shouldBe` [("fghij", "fguij")]
