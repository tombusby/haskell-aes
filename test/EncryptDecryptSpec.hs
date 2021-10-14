module EncryptDecryptSpec
  ( runTests,
  )
where

import Control.Exception (evaluate)
import Data.Maybe (fromJust)
import EncryptDecrypt
  ( decrypt,
    decryptBlocksCBC,
    decryptBlocksECB,
    encrypt,
    encryptBlocksCBC,
    encryptBlocksECB,
  )
import Globals (Block, Key)
import Test.Hspec (describe, hspec, it, shouldBe)

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

round10Output :: Key
round10Output =
  [ 0x3A,
    0xD7,
    0x7B,
    0xB4,
    0x0D,
    0x7A,
    0x36,
    0x60,
    0xA8,
    0x9E,
    0xCA,
    0xF3,
    0x24,
    0x66,
    0xEF,
    0x97
  ]

runTests :: IO ()
runTests = hspec $
  describe "EncryptDecryptSpec Module" $ do
    describe "encrypt/decrypt primitives" $ do
      describe "encrypt" $
        it "ensures that encrypting with a given key produces the expected output" $
          encrypt key block1 `shouldBe` round10Output
      describe "decrypt" $
        it "ensures that decrypting with a given key produces the expected output" $
          decrypt key round10Output `shouldBe` block1
      describe "encrypt/decrypt" $
        it "ensures that encryption and decryption are inverse operations" $ do
          (decrypt key . encrypt key) block1 `shouldBe` block1
          (encrypt key . decrypt key) block1 `shouldBe` block1
          (decrypt block1 . encrypt block1) key `shouldBe` key
          (encrypt block1 . decrypt block1) key `shouldBe` key
    describe "ECB mode" $
      it "ensures that ECB encryption and ECB decryption are (not including Maybe) inverse operations" $ do
        -- All blocks are a multiple of blockSize (so have full padding block as final block)
        encryptDecryptECB key [block1] `shouldBe` [block1]
        encryptDecryptECB block1 [key] `shouldBe` [key]
        encryptDecryptECB key [block1, key] `shouldBe` [block1, key]
        encryptDecryptECB block1 [key, key] `shouldBe` [key, key]
        -- Final block is not a multiple of blockSize (final block is padded and unpadded)
        encryptDecryptECB key [block1, [0, 1]] `shouldBe` [block1, [0, 1]]
        encryptDecryptECB block1 [key, [0, 1]] `shouldBe` [key, [0, 1]]
    describe "CBC mode" $
      it "ensures that CBC encryption and CBC decryption are (not including Maybe) inverse operations" $ do
        let iv = round10Output
        -- All blocks are a multiple of blockSize (so have full padding block as final block)
        encryptDecryptCBC key iv [block1] `shouldBe` [block1]
        encryptDecryptCBC iv block1 [key] `shouldBe` [key]
        encryptDecryptCBC key iv [block1, iv] `shouldBe` [block1, iv]
        encryptDecryptCBC iv block1 [iv, key] `shouldBe` [iv, key]
        -- Final block is not a multiple of blockSize (final block is padded and unpadded)
        encryptDecryptCBC key iv [block1, [0, 1]] `shouldBe` [block1, [0, 1]]
        encryptDecryptCBC iv block1 [key, [0, 1]] `shouldBe` [key, [0, 1]]
  where
    encryptDecryptECB key = fromJust . decryptBlocksECB key . encryptBlocksECB key
    encryptDecryptCBC key iv = fromJust . decryptBlocksCBC key iv . encryptBlocksCBC key iv
