module Problem1Spec (spec) where

import Test.Hspec
import Problem1

spec :: Spec
spec = do
    describe "Problem 1" $ do
        it "should return the total frequency" $
            lastFrequency "1\n2\n-1\n+1\n" `shouldBe` 3

    describe "Problem 2" $ do
        it "should return the first repeated frequency" $
            firstRepeatedFrequency "3\n3\n4\n-2\n-4" `shouldBe` 10


    describe "Frequencies" $ do
        it "should work" $
            frequencies [1,2] `shouldBe` [0,1,3]

        it "should be able to find repeated frequency" $
            (frequencies) [3, 3, 4, -2, -4] `shouldBe` [0, 3, 6, 10, 8, 4]

    describe "Parsing a number" $ do
        it "should parse a simple integer" $
            parse "3" `shouldBe` 3
        it "should parse an integer with a preceeding '+'" $
            parse "+3" `shouldBe` 3
        it "should parse an integer with a preceeding '-'" $
            parse "-3" `shouldBe` -3

    describe "Find repeated frequency" $ do
        it "should be able to find repeated frequency" $
            (firstRepeatedFrequencyIn . frequencies) [1, 2, -1, 1] `shouldBe` 3
        it "should be able to find repeated frequency 2" $
            (firstRepeatedFrequencyIn . frequencies) [1, -1] `shouldBe` 0
        it "should be able to find repeated frequency 3" $
            (firstRepeatedFrequencyIn . frequencies . cycle) [3, 3, 4, -2, -4] `shouldBe` 10
