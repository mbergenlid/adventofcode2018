module Problem6Spec (spec) where

import Test.Hspec
import Problem6


spec :: Spec
spec = do
    describe "Areas" $ do
        it "Should work" $
            largestArea [(1,1), (1,6), (8,3), (3,4), (5,5), (8,9)] `shouldBe` 17

        it "Should work again" $
            largestArea [(1,1), (9,1), (1,8), (8,7), (9,8)] `shouldBe` 16
            -- Aaaa.bbbB
            -- aaaa.bbbb
            -- aaaa.bbbb
            -- aaaaeeeeb
            -- ccc.eeee.
            -- ccc.eeee.
            -- ccc.eeeE.
            -- Cccc....D


-- aaaaa.cccc
-- aAaaa.cccc
-- aaaddecccc
-- aadddeccCcccccccccccccccccc
-- ..dDdeeccg
-- bb.deEe.gGgggggggggggg
-- bBb.eeee..
-- bbb.eeefff
-- bbb.eeffff
-- bbb.ffffFf
    describe "Bounded Rectangle" $ do
        it "Should give the bounded rectangle from a list of locations" $
            boundedRect [(1,1), (1,6), (8,3), (3,4), (5,5), (8,9)] `shouldBe` (Rect (1,1) 7 8)

        it "Can iterate over all Locations in a Rect" $
            locationsIn (Rect (1,1) 1 1) `shouldBe` [(1,1),(1,2),(2,1),(2,2)]

    describe "Location" $ do
        it "Can calculate manhattan distance to another location on the same column" $
            manhattanDistance (1,1) (1,6) `shouldBe` 5

        it "Can calculate manhattan distance to another location on the same row" $
            manhattanDistance (1,1) (6,1) `shouldBe` 5

        it "Can calculate manhattan distance to another location" $
            manhattanDistance (1,1) (8,3) `shouldBe` 9

        it "Can calculate manhattan distance to another location" $
            manhattanDistance (8,3) (1,1) `shouldBe` 9

        it "Can calculate manhattan distance to another location" $
            manhattanDistance (8,1) (1,3) `shouldBe` 9


    describe "All safe areas" $ do
        it "should work" $
            length (allSafeAreas 32 [(1,1),(1,6),(8,3),(3,4),(5,5),(8,9)]) `shouldBe`16
