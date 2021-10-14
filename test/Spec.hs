import EncryptDecryptSpec (runTests)
import PaddingSpec (runTests)
import RoundInternalSpec (runTests)
import RoundSpec (runTests)
import SBoxSpec (runTests)
import SubKeyGenSpec (runTests)

main :: IO ()
main = do
  SBoxSpec.runTests
  PaddingSpec.runTests
  RoundInternalSpec.runTests
  RoundSpec.runTests
  SubKeyGenSpec.runTests
  EncryptDecryptSpec.runTests
