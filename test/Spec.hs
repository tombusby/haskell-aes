import SBoxSpec (runTests)
import PaddingSpec (runTests)

main :: IO ()
main = do
    SBoxSpec.runTests
    PaddingSpec.runTests
