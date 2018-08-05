module Round.Internal where

import Data.Bits
import Data.List
import Data.List.Split

import Globals (Key, Block)
import SBox

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
