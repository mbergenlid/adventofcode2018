module Problem3Spec (spec) where

import Test.Hspec
import Problem3

spec :: Spec
spec = do
    describe "Rectangle" $ do
        it "can return intersection of another" $
            -- . . . . . .
            -- . 1 X X X 2
            -- . 1 X X X 2
            -- . 1 X X X 2
            -- . 1 X X X 2
            (Rect 1 1 4 4) `intersection` (Rect 2 1 4 4) `shouldBe` Just (Rect 2 1 3 4)

        it "can return intersection of another 2" $
            -- . . . . . .
            -- . 1 1 1 1 1
            -- . 1 X X X 2
            -- . 1 X X X 2
            -- . 1 X X X 2
            -- . . 2 2 2 2
            (Rect 1 1 4 4) `intersection` (Rect 2 2 4 4) `shouldBe` Just (Rect 2 2 3 3)

        it "can return intersection of another 3" $
            -- . . . . . .
            -- . 2 X X X 1
            -- . 2 X X X 1
            -- . 2 X X X 1
            -- . 2 X X X 1
            -- . . . . . .
            (Rect 2 1 4 4) `intersection` (Rect 1 1 4 4) `shouldBe` Just (Rect 2 1 3 4)

        it "can return intersection of another 4" $
            -- . . . . . .
            -- . 2 2 2 2 2
            -- . 2 X X X 1
            -- . 2 X X X 1
            -- . 2 X X X 1
            -- . . 1 1 1 1
            (Rect 2 2 4 4) `intersection` (Rect 1 1 4 4) `shouldBe` Just (Rect 2 2 3 3)

        it "can return intersection of another 5" $
            -- . . . . . .
            -- . . 2 2 2 2
            -- . 1 X X X 2
            -- . 1 X X X 2
            -- . 1 1 1 1 .
            -- . 1 1 1 1 .
            (Rect 1 2 4 4) `intersection` (Rect 2 1 4 3) `shouldBe` Just (Rect 2 2 3 2)

        it "returns Nothing" $
            -- . . . . . .
            -- . . . . 2 2
            -- . 1 1 . 2 2
            -- . 1 1 . 2 2
            -- . . . . . .
            (Rect 1 2 2 2) `intersection` (Rect 4 1 2 3) `shouldBe` Nothing

        it "has an area" $
            area (Rect 1 2 3 4) `shouldBe` 12

        it "can check if it contains a point" $
            -- . . . . . .
            -- . 1 1 1 1 .
            -- . 1 X 1 1 .
            -- . 1 1 1 1 .
            -- . 1 1 1 1 .
            contains (Point 2 2) (Rect 1 1 4 4) `shouldBe` True

        it "can check if it contains a point" $
            -- . . X . . .
            -- . 1 1 1 1 .
            -- . 1 1 1 1 .
            -- . 1 1 1 1 .
            -- . 1 1 1 1 .
            contains (Point 2 0) (Rect 1 1 4 4) `shouldBe` False

    describe "A Claim" $ do
        it "can be read from a string" $
            read "#2 @ 1,3: 4x4" `shouldBe` (Claim 2 (Rect 1 3 4 4))

    describe "Square inch within two or more claims" $ do
        it "should work" $
            -- . . . . .
            -- . . . 2 2 2 2
            -- . . . 2 2 2 2
            -- . 1 1 x x 2 2
            -- . 1 1 x x 2 2
            -- . 1 1 1 1
            -- . 1 1 1 1
            squaresInTwoOrMore [Claim 1 (Rect 1 3 4 4), Claim 2 (Rect 3 1 4 4)] `shouldBe` 4

    describe "Overlap" $ do
        it "should work" $
            -- . . . . .
            -- . . . 2 2 2 2
            -- . . . 2 2 2 2
            -- . 1 1 x x 2 2
            -- . 1 1 x x 2 2
            -- . 1 1 1 1
            -- . 1 1 1 1
            doesNotOverlapAnyOther [Claim 1 (Rect 1 3 4 4), Claim 2 (Rect 3 1 4 4), Claim 3 (Rect 5 5 2 2)] `shouldBe` [Claim 3 (Rect 5 5 2 2)]
