module Round.Internal where

import Data.Word (Word8)
import Data.Bits (xor)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Crypto.Number.F2m (BinaryPolynomial, addF2m, mulF2m)

import Globals (Key, Block)
import SBox

aes_polynomial :: BinaryPolynomial
aes_polynomial = 0x11B

byteSub :: Block -> Block
byteSub = map SBox.sBox

shiftRows :: Block -> Block
shiftRows = concat . transpose . rotate . transpose . chunksOf 4
    where
        rotate = zipWith rotateRow [0, 1, 2, 3]
        rotateRow n xs = take (length xs) (drop n (cycle xs))

mixColumns :: Block -> Block
mixColumns = id

keyAdd :: Key -> Block -> Block
keyAdd = zipWith xor
