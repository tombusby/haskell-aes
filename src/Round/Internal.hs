module Round.Internal
    ( keyAdd,
      byteSub,
      byteSubInv,
      shiftRows,
      shiftRowsInv,
      mixColumns,
      mixColumnsInv,
    ) where

import Data.Word (Word8)
import Data.Bits (xor)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Crypto.Number.F2m (addF2m, mulF2m)

import Globals (Key, Block, chunkSize, aesPolynomial)
import SBox (sBox, sBoxInv)

keyAdd :: Key -> Block -> Block
keyAdd = zipWith xor

byteSub :: Block -> Block
byteSub = map sBox

byteSubInv :: Block -> Block
byteSubInv = map sBoxInv

shiftRows :: Block -> Block
shiftRows = srCommon [0, 1, 2, 3]

shiftRowsInv :: Block -> Block
shiftRowsInv = srCommon [0, 3, 2, 1]

srCommon :: [Int] -> Block -> Block
srCommon shifts = concat . transpose . rotate . transpose . chunksOf chunkSize
    where
        rotate = zipWith rotateRow shifts
        rotateRow n xs = take (length xs) (drop n (cycle xs))

mixColumns :: Block -> Block
mixColumns = mcCommon matrixConsts
    where
        matrixConsts = [[2, 3, 1, 1], [1, 2, 3, 1], [1, 1, 2, 3], [3, 1, 1, 2]]

mixColumnsInv :: Block -> Block
mixColumnsInv = mcCommon matrixConsts
    where
        matrixConsts = [[0x0E, 0x0B, 0x0D, 0x09], [0x09, 0x0E, 0x0B, 0x0D],
            [0x0D, 0x09, 0x0E, 0x0B], [0x0B, 0x0D, 0x09, 0x0E]]

mcCommon :: [[Integer]] -> Block -> Block
mcCommon consts = map fromIntegral . concat . matrixMults . chunksOf chunkSize
    where
        matrixMults b = [zipWith doMults (repeat r) consts | r <- b]
        doMults l = foldr addF2m 0 . zipWith multWord l
        multWord = mulF2m aesPolynomial . fromIntegral
