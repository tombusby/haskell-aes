module Round
    ( roundEncrypt,
      finalRoundEncrypt,
      roundDecrypt,
      finalRoundDecrypt,
    ) where

import Globals (Key, Block)
import Round.Internal (keyAdd, byteSub, shiftRows, mixColumns,
        byteSubInv, shiftRowsInv, mixColumnsInv)

roundEncrypt :: Key -> Block -> Block
roundEncrypt k = keyAdd k . mixColumns . shiftRows . byteSub

finalRoundEncrypt :: Key -> Block -> Block
finalRoundEncrypt k = keyAdd k . shiftRows . byteSub

roundDecrypt :: Key -> Block -> Block
roundDecrypt k = byteSubInv . shiftRowsInv . mixColumnsInv . keyAdd k

finalRoundDecrypt :: Key -> Block -> Block
finalRoundDecrypt k = byteSubInv . shiftRowsInv . keyAdd k
