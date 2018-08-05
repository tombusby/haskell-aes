module Round
    ( roundEncrypt,
      roundDecrypt,
    ) where

import Globals (Key, Block, blockSize, numRounds)
import Round.Internal (keyAdd, byteSub, shiftRows, mixColumns,
        byteSubInv, shiftRowsInv, mixColumnsInv)

roundEncrypt :: Key -> Block -> Block
roundEncrypt k = keyAdd k . mixColumns . shiftRows . byteSub

roundDecrypt :: Key -> Block -> Block
roundDecrypt k = byteSubInv . shiftRowsInv . mixColumnsInv . keyAdd k
