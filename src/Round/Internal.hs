module Round.Internal where

import Data.Word (Word8)
import Data.Bits (xor)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Crypto.Number.F2m (BinaryPolynomial, addF2m, mulF2m)

import Globals (Key, Block)
import SBox (sBox, sBoxInv)

matrixMultsConsts :: Num a => [[a]]
matrixMultsConsts = [[2, 3, 1, 1],[1, 2, 3, 1],[1, 1, 2, 3],[3, 1, 1, 2]]

byteSub :: Block -> Block
byteSub = map sBox

shiftRows :: Block -> Block
shiftRows = concat . transpose . rotate . transpose . chunksOf 4
    where
        rotate = zipWith rotateRow [0, 1, 2, 3]
        rotateRow n xs = take (length xs) (drop n (cycle xs))

mixColumns :: Block -> Block
mixColumns = map fromIntegral . concat . matrixMults . chunksOf 4
    where
        matrixMults b = [zipWith doMults (repeat r) matrixMultsConsts | r <- b]
        doMults l = foldr addF2m 0 . zipWith multWord l
        multWord = mulF2m aesPolynomial . fromIntegral
        aesPolynomial = 0x11B

keyAdd :: Key -> Block -> Block
keyAdd = zipWith xor
