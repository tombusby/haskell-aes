module RoundSpec
  ( runTests,
  )
where

import Control.Exception (evaluate)
import Globals (Block, Key)
import Round (roundDecrypt, roundEncrypt)
import Test.Hspec (describe, hspec, it, pendingWith, shouldBe)
import Test.QuickCheck ()

initialKeyAddOutput1 :: Block
initialKeyAddOutput1 =
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

roundOutput1 :: Block
roundOutput1 =
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
  describe "Round Module" $ do
    describe "roundEncrypt" $
      it "ensures that roundEncrypt produces correct output" $ do
        roundEncrypt subKey1 initialKeyAddOutput1 `shouldBe` roundOutput1
        pendingWith "need to add tests for finalRoundEncrypt"
    describe "roundDecrypt" $
      it "ensures that roundDecrypt produces correct output" $ do
        roundDecrypt subKey1 roundOutput1 `shouldBe` initialKeyAddOutput1
        pendingWith "need to add tests for finalRoundDecrypt"
    describe "check inverse holds" $
      it "checks that round encryption and decryption are inverses of each other" $ do
        encryptDecrypt initialKeyAddOutput1 `shouldBe` initialKeyAddOutput1
        decryptEncrypt initialKeyAddOutput1 `shouldBe` initialKeyAddOutput1
  where
    encryptDecrypt = roundDecrypt subKey1 . roundEncrypt subKey1
    decryptEncrypt = roundEncrypt subKey1 . roundDecrypt subKey1
