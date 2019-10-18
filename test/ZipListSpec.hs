module ZipListSpec (spec) where

import Test.Hspec
import ZipList

spec :: Spec
spec = do
    describe "A ZipList" $ do
        it "it can be created" $
            current (fromList [0]) `shouldBe` 0

        it "can rotate single element list" $
            (current . moveRight . moveRight) (fromList [0]) `shouldBe` 0

        it "we can be navigated" $
            (current . moveRight) (fromList [0, 1, 2, 3, 4]) `shouldBe` 1

        it "we can be navigated multiple times" $
            (current . moveRight . moveRight . moveRight) (fromList [0, 1, 2, 3, 4]) `shouldBe` 3

        it "should wrap around" $
            (current . moveRight . moveRight . moveRight) (fromList [0, 1, 2]) `shouldBe` 0

        it "can insert values at current position" $
            (toList . (insert 5) . moveRight . moveRight) (fromList [0,1,2,3,4])  `shouldBe` [0,1,5,2,3,4]

        it "can insert values at start" $
            (toList . (insert 5)) (fromList [0,1,2,3,4])  `shouldBe` [5,0,1,2,3,4]

        it "can insert values at end" $
            (toList . (insert 5) . (apply 4 moveRight)) (fromList [0,1,2,3,4])  `shouldBe` [0,1,2,3,5,4]

        it "can remove value" $
            (toList . remove . (apply 4 moveRight)) (fromList [0,1,2,3,4])  `shouldBe` [0,1,2,3]

        it "can move left" $
            (current . moveLeft) (fromList [0, 1, 2, 3, 4]) `shouldBe` 4

        it "can move left twice" $
            (current . moveLeft .moveLeft) (fromList [0, 1, 2, 3, 4]) `shouldBe` 3

        it "can remove value" $
            (current . remove . (apply 4 moveRight)) (fromList [0,1,2,3,4])  `shouldBe` 0

        it "can remove value" $
            (current . remove . (apply 3 moveRight)) (fromList [0,1,2,3,4])  `shouldBe` 4


apply :: Int -> (a -> a) -> a -> a
apply x f initial = foldl (\accum i -> f accum) initial [1..x]
