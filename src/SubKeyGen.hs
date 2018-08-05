module SubKeyGen
    ( generateSubKeys
    ) where

import Data.Word
import Crypto.Number.F2m (modF2m)

import Globals (Key, numRounds, aesPolynomial)

type KeyWord = [Word8]
type RC = [Word8]

generateSubKeys :: Key -> [Key]
generateSubKeys = undefined

g :: RC -> KeyWord -> KeyWord
g = undefined
