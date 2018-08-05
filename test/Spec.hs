import SBoxSpec (runTests)
import PaddingSpec (runTests)
import RoundInternalSpec (runTests)
import RoundSpec (runTests)

main :: IO ()
main = do
    SBoxSpec.runTests
    PaddingSpec.runTests
    RoundInternalSpec.runTests
    RoundSpec.runTests
