module Round.Internal where

import Data.Bits

import Globals (Key, Block)
import SBox

byteSub :: Block -> Block
byteSub = map SBox.sBox

shiftRows :: Block -> Block
shiftRows = id

mixColumns :: Block -> Block
mixColumns = id

keyAdd :: Key -> Block -> Block
keyAdd = zipWith xor
