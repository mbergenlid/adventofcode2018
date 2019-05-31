module StringSpec (spec) where

import Test.Hspec
import Strings

spec :: Spec
spec = do
    describe "Drop all from string" $ do
        it "Can drop 1 string" $
            dropAll [2] "Hello" `shouldBe` "Heo"

        it "Can drop 2 string" $
            dropAll [1,7] "Hello, world!" `shouldBe` "Hlo, rld!"
