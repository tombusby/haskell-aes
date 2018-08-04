import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $
    describe "Test" $ do
        it "checks that the hspec deps are working" $
            3 `shouldBe` (2 + 1)
        it "checks that QuickCheck is working" $
            property $ \x -> id x == (x :: Int)
