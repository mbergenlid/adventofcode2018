module Problem8Spec (spec) where

import Test.Hspec
import Problem8


spec :: Spec
spec = do
    describe "Areas" $ do
        it "Should work" $
            metadataSum [0, 3, 10, 11, 12] `shouldBe` (33, [])

        it "should work again" $
            metadataSum [1, 3, 0, 3, 10, 11, 12, 1, 2, 3] `shouldBe` (39, [])


    describe "Node values" $ do
        it "node with no children should sum up the metadata entries" $
            nodeValue [0, 3, 10, 11, 12] `shouldBe` (33, [])

        it "node with one child should use the index etc" $
            nodeValue [1, 3, 0, 3, 10, 11, 12, 1, 2, 3] `shouldBe` (33, [])

        it "node with zero metadata" $
            nodeValue [1, 2, 0, 3, 10, 11, 12, 0, 2] `shouldBe` (0, [])

        it "node with 2 children" $
            nodeValue [2, 2, 0, 3, 10, 11, 12, 0, 1, 10, 1, 2] `shouldBe` (43, [])

        it "should work on test input" $
            nodeValue [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2] `shouldBe` (66, [])

