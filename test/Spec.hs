import SBoxSpec (runTests)
import PaddingSpec (runTests)
import RoundInternalSpec (runTests)
import RoundSpec (runTests)
import SubKeyGenSpec (runTests)
import EncryptDecryptSpec (runTests)

main :: IO ()
main = do
    SBoxSpec.runTests
    PaddingSpec.runTests
    RoundInternalSpec.runTests
    RoundSpec.runTests
    SubKeyGenSpec.runTests
    EncryptDecryptSpec.runTests
