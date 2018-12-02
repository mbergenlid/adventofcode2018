module Problem1Spec (spec) where

import Test.Hspec
import Problem1

spec :: Spec
spec = do
    describe "Frequencies" $ do
        it "should sum up all numbers" $
           frequency [1,2] `shouldBe` 3

        it "should be able to sum up a list of strings" $
            frequencyStrings ["1", "2"] `shouldBe` 3

        it "should be able to sum up a list of strings starting with +" $
            frequencyStrings ["-1", "+2"] `shouldBe` 1
