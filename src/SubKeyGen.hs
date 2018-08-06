module SubKeyGen
    ( generateSubKeys
    ) where

import Data.Word
import Data.Bits (xor)
import Crypto.Number.F2m (modF2m)

import Globals (Key, numRounds, aesPolynomial)
import SBox(sBox)

type KeyWord = [Word8]
type RC = Word8

generateSubKeys :: Key -> [Key]
generateSubKeys = undefined

rcVals :: [RC]
rcVals = map (fromIntegral . modF2m aesPolynomial) (take numRounds $ doubles 1)
    where
        doubles n = n:(doubles $ 2 * n)

g :: RC -> KeyWord -> KeyWord
g rc kw = rcXor . map sBox $ kwRot
    where
        kwRot = let (l, r) = splitAt 1 kw in r ++ l
        rcXor (x:xs) = (rc `xor` x):xs
