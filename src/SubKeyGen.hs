module SubKeyGen
    ( generateSubKeys
    ) where

import Data.Word
import Crypto.Number.F2m (modF2m)

import Globals (Key, numRounds, aesPolynomial)

type KeyWord = [Word8]
type RC = [Word8]

rcVals :: [Word8]
rcVals = map (fromIntegral . modF2m aesPolynomial) (take numRounds $ doubles 1)
    where
        doubles n = n:(doubles $ 2 * n)

generateSubKeys :: Key -> [Key]
generateSubKeys = undefined

g :: RC -> KeyWord -> KeyWord
g = undefined
