module SubKeyGenSpec
    ( runTests
    ) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Globals (Key)
import SubKeyGen

key :: Key
key = [0x2B, 0x7E, 0x15, 0x16, 0x28, 0xAE, 0xD2, 0xA6,
    0xAB, 0xF7, 0x15, 0x88, 0x09, 0xCF, 0x4F, 0x3C]

subKey1 :: Key
subKey1 = [0xa0, 0xfa, 0xfe, 0x17, 0x88, 0x54, 0x2c, 0xb1,
    0x23, 0xa3, 0x39, 0x39, 0x2a, 0x6c, 0x76, 0x05]

runTests :: IO ()
runTests = hspec $ do
    describe "generateSubKeys" $ do
        it "checks that, given a key, it prodeces the correct list of subkeys" $
            ((!!0) . generateSubKeys $ key) `shouldBe` subKey1
