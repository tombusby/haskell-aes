import SBoxSpec (runTests)
import PaddingSpec (runTests)
import RoundSpec (runTests)

main :: IO ()
main = do
    SBoxSpec.runTests
    PaddingSpec.runTests
    RoundSpec.runTests
