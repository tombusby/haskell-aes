module RoundSpec
    ( runTests
    ) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Round

runTests :: IO ()
runTests = hspec $ do
    describe "Round Module" $ do
        describe "roundEncrypt" $ do
            it "ensures that roundEncrypt produces correct output" $ do
                pendingWith "need to write and test internal library first"
        describe "roundDecrypt" $ do
            it "ensures that roundDecrypt produces correct output" $ do
                pendingWith "need to write and test internal library first"
