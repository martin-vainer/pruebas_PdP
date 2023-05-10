module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  --Test del punto 1
  describe "el doble de un numero" $ do 
    it "el doble de 2" $ do
      doble 2 `shouldBe` 4