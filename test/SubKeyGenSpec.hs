module SubKeyGenSpec
    ( runTests
    ) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

runTests :: IO ()
runTests = hspec $ do
    describe "generateSubKeys" $ do
        it "checks that, given a key, it prodeces the correct list of subkeys" $
            pendingWith "need to implement keygen"
