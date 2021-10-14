module RoundInternalSpec
  ( runTests,
  )
where

import Control.Exception (evaluate)
import Globals (Block, Key)
import Round.Internal
import Test.Hspec
import Test.QuickCheck

block1 :: Block
block1 =
  [ 0x6B,
    0xC1,
    0xBE,
    0xE2,
    0x2E,
    0x40,
    0x9F,
    0x96,
    0xE9,
    0x3D,
    0x7E,
    0x11,
    0x73,
    0x93,
    0x17,
    0x2A
  ]

key :: Key
key =
  [ 0x2B,
    0x7E,
    0x15,
    0x16,
    0x28,
    0xAE,
    0xD2,
    0xA6,
    0xAB,
    0xF7,
    0x15,
    0x88,
    0x09,
    0xCF,
    0x4F,
    0x3C
  ]

initialKeyAddOutput :: Block
initialKeyAddOutput =
  [ 0x40,
    0xBF,
    0xAB,
    0xF4,
    0x06,
    0xEE,
    0x4D,
    0x30,
    0x42,
    0xCA,
    0x6B,
    0x99,
    0x7A,
    0x5C,
    0x58,
    0x16
  ]

subKey1 :: Key
subKey1 =
  [ 0xa0,
    0xfa,
    0xfe,
    0x17,
    0x88,
    0x54,
    0x2c,
    0xb1,
    0x23,
    0xa3,
    0x39,
    0x39,
    0x2a,
    0x6c,
    0x76,
    0x05
  ]

byteSubOutput1 :: Block
byteSubOutput1 =
  [ 0x09,
    0x08,
    0x62,
    0xBF,
    0x6F,
    0x28,
    0xE3,
    0x04,
    0x2C,
    0x74,
    0x7F,
    0xEE,
    0xDA,
    0x4A,
    0x6A,
    0x47
  ]

shiftRowsOutput1 :: Block
shiftRowsOutput1 =
  [ 0x09,
    0x28,
    0x7F,
    0x47,
    0x6F,
    0x74,
    0x6A,
    0xBF,
    0x2C,
    0x4A,
    0x62,
    0x04,
    0xDA,
    0x08,
    0xE3,
    0xEE
  ]

mixColumnsOutput1 :: Block
mixColumnsOutput1 =
  [ 0x52,
    0x9F,
    0x16,
    0xC2,
    0x97,
    0x86,
    0x15,
    0xCA,
    0xE0,
    0x1A,
    0xAE,
    0x54,
    0xBA,
    0x1A,
    0x26,
    0x59
  ]

keyAddOutput1 :: Block
keyAddOutput1 =
  [ 0xF2,
    0x65,
    0xE8,
    0xD5,
    0x1F,
    0xD2,
    0x39,
    0x7B,
    0xC3,
    0xB9,
    0x97,
    0x6D,
    0x90,
    0x76,
    0x50,
    0x5C
  ]

runTests :: IO ()
runTests = hspec $ do
  describe "Round.Internal Module" $ do
    describe "InitialKeyAdd" $ do
      it "ensures that the initial keyAdd functions correctly" $
        keyAdd key block1 `shouldBe` initialKeyAddOutput
      it "ensures that the initial keyAdd functions correctly" $
        keyAdd key initialKeyAddOutput `shouldBe` block1
    describe "ByteSub" $ do
      it "ensures that the byteSub SBox layer functions correctly" $
        byteSub initialKeyAddOutput `shouldBe` byteSubOutput1
      it "ensures that the inverse byteSub SBox layer functions correctly" $
        byteSubInv byteSubOutput1 `shouldBe` initialKeyAddOutput
    describe "ShiftRows" $ do
      it "ensures that the shiftRows layer functions correctly" $
        shiftRows byteSubOutput1 `shouldBe` shiftRowsOutput1
      it "ensures that the inverse shiftRows layer functions correctly" $
        shiftRowsInv shiftRowsOutput1 `shouldBe` byteSubOutput1
    describe "MixColumn" $ do
      it "ensures that the mixColumns layer functions correctly" $
        mixColumns shiftRowsOutput1 `shouldBe` mixColumnsOutput1
      it "ensures that the inverse mixColumns layer functions correctly" $
        mixColumnsInv mixColumnsOutput1 `shouldBe` shiftRowsOutput1
    describe "KeyAdd" $ do
      it "ensures that the keyAdd layer functions correctly" $
        keyAdd subKey1 mixColumnsOutput1 `shouldBe` keyAddOutput1
      it "ensures that the inverse keyAdd layer functions correctly" $
        keyAdd subKey1 keyAddOutput1 `shouldBe` mixColumnsOutput1
