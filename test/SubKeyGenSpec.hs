module SubKeyGenSpec
  ( runTests,
  )
where

import Control.Exception (evaluate)
import Globals (Key)
import SubKeyGen (generateSubKeys)
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck ()

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

subKey4 :: Key
subKey4 =
  [ 0xEF,
    0x44,
    0xA5,
    0x41,
    0xA8,
    0x52,
    0x5B,
    0x7F,
    0xB6,
    0x71,
    0x25,
    0x3B,
    0xDB,
    0x0B,
    0xAD,
    0x00
  ]

subKey9 :: Key
subKey9 =
  [ 0xAC,
    0x77,
    0x66,
    0xF3,
    0x19,
    0xFA,
    0xDC,
    0x21,
    0x28,
    0xD1,
    0x29,
    0x41,
    0x57,
    0x5C,
    0x00,
    0x6E
  ]

subKey10 :: Key
subKey10 =
  [ 0xD0,
    0x14,
    0xF9,
    0xA8,
    0xC9,
    0xEE,
    0x25,
    0x89,
    0xE1,
    0x3F,
    0x0C,
    0xC8,
    0xB6,
    0x63,
    0x0C,
    0xA6
  ]

runTests :: IO ()
runTests = hspec $ do
  describe "SubKeyGen Module" $
    describe "generateSubKeys" $
      it "checks that, given a key, it prodeces the correct list of subkeys" $ do
        ((!! 0) . generateSubKeys $ key) `shouldBe` subKey1
        ((!! 3) . generateSubKeys $ key) `shouldBe` subKey4
        ((!! 8) . generateSubKeys $ key) `shouldBe` subKey9
        ((!! 9) . generateSubKeys $ key) `shouldBe` subKey10
