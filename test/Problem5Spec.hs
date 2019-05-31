module Problem5Spec (spec) where

import Test.Hspec
import Problem5

spec :: Spec
spec = do
    describe "Polymer" $ do
        it "Simple example 1" $
            polymer "aA" `shouldBe` polymer ""

        it "Simple example 2" $
            polymer "abBA" `shouldBe` polymer ""
        it "Simple example 3" $
            units (polymer "abAB") `shouldBe` 4
        it "Simple example 4" $
            units (polymer "aabAAB") `shouldBe` 6

        it "Polymer that can not react" $
            units (polymer "dabCBBccaDA") `shouldBe` 11

        it "Polymer that can react once" $
            units (polymer "dabCBAcCcaDA") `shouldBe` 10

        it "Polymer that can react twice" $
            units (polymer "dabAaCBAcCcaDA") `shouldBe` 10

        it "Polymer that reacts in the end" $
            units (polymer "dabaA") `shouldBe` 3

        it "Polymer that reacts in the beginning" $
            units (polymer "dDabaA") `shouldBe` 2

        it "Polymer more advanced" $
            units (polymer "WwWwKopPhHOkKkiaAInJjNjxAZzAaaFeEMmFffHOJjWwlMmvVEeeEPptMmxdDGgatnNTGUugjJcCimMIbBnVxXvsVvVpPvxXuuEiIeUVvILlfFiUPdDpYyiISK") `shouldBe` 10
            --jxHOltxanK

        it "Polymer more advanced 2" $
            units (polymer "JLljBbTtzShHsZRBbEelLOoFfiyYIZzeELlBbuUrTtaIiXHtThFoOfzZkKNOojJpPXxrxXRcVvmbBTTttMEeQgGqpmMTtsSGgPCnknNKrRPPpeEpOoTtKkwWttTAaPTtpTjJqQXnNhHxDdME") `shouldBe` 4
            --JLljBbTtzShHsZRBbEelLOoFfiyYIZzeELlBbuUrTtaIiXHtThFoOfzZkKNOojJpPXxrxXRcVvmbBTTttMEeQgGqpmMTtsSGgPCnknNKrRPPpeEpOoTtKkwWttTAaPTtpTjJqQXnNhHxDdME
            --
    describe "Shortest polymer" $
        it "Works on example" $
            (units . shortestPolymer) "dabAcCaCBAcCcaDA" `shouldBe` 4
