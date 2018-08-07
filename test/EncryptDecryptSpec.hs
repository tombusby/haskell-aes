module EncryptDecryptSpec
    ( runTests
    ) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Globals (Key, Block)
import EncryptDecrypt (encrypt, decrypt)

block1 :: Block
block1 = [0x6B, 0xC1, 0xBE, 0xE2, 0x2E, 0x40, 0x9F, 0x96,
    0xE9, 0x3D, 0x7E, 0x11, 0x73, 0x93, 0x17, 0x2A]

key :: Key
key = [0x2B, 0x7E, 0x15, 0x16, 0x28, 0xAE, 0xD2, 0xA6,
    0xAB, 0xF7, 0x15, 0x88, 0x09, 0xCF, 0x4F, 0x3C]

round10Output :: Key
round10Output = [0x3A, 0xD7, 0x7B, 0xB4, 0x0D, 0x7A, 0x36, 0x60,
    0xA8, 0x9E, 0xCA, 0xF3, 0x24, 0x66, 0xEF, 0x97]

runTests :: IO ()
runTests = hspec $
    describe "EncryptDecryptSpec Module" $ do
        describe "encrypt" $
            it "ensures that encrypting with a given key produces the expected output" $
                encrypt key block1 `shouldBe` round10Output
        describe "decrypt" $ 
            it "ensures that decrypting with a given key produces the expected output" $
                decrypt key round10Output `shouldBe` block1
        describe "encrypt/decrypt" $  
            it "ensures that encryption and decryption are inverse operations" $ do
                (decrypt key . encrypt key $ block1) `shouldBe` block1
                (encrypt key . decrypt key $ block1) `shouldBe` block1
                (decrypt block1 . encrypt block1 $ key) `shouldBe` key
                (encrypt block1 . decrypt block1 $ key) `shouldBe` key
