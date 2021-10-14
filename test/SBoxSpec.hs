module SBoxSpec
  ( runTests,
  )
where

import Control.Exception (evaluate)
import SBox
import Test.Hspec
import Test.QuickCheck

runTests :: IO ()
runTests = hspec $
  describe "Sbox Module" $ do
    it "tests some specifc sBox outputs for correctness" $ do
      sBox 0x21 `shouldBe` 0xfd
      sBox 0xba `shouldBe` 0xf4
      sBox 0x5b `shouldBe` 0x39
      sBox 0x77 `shouldBe` 0xf5
    it "tests some specifc sBoxInv outputs for correctness" $ do
      sBoxInv 0xba `shouldBe` 0xc0
      sBoxInv 0x5c `shouldBe` 0xa7
      sBoxInv 0xe2 `shouldBe` 0x3b
      sBoxInv 0x0f `shouldBe` 0xfb
    it "ensures that sBox and sBoxInv are truly inverses" $
      property $ \x ->
        all ((== x) . ($x)) [sBoxInv . sBox, sBox . sBoxInv]
