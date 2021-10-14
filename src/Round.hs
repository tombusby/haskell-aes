module Round
  ( roundEncrypt,
    finalRoundEncrypt,
    roundDecrypt,
    finalRoundDecrypt,
  )
where

import Globals (Block, Key)
import Round.Internal
  ( byteSub,
    byteSubInv,
    keyAdd,
    mixColumns,
    mixColumnsInv,
    shiftRows,
    shiftRowsInv,
  )

roundEncrypt :: Key -> Block -> Block
roundEncrypt k = keyAdd k . mixColumns . shiftRows . byteSub

finalRoundEncrypt :: Key -> Block -> Block
finalRoundEncrypt k = keyAdd k . shiftRows . byteSub

roundDecrypt :: Key -> Block -> Block
roundDecrypt k = byteSubInv . shiftRowsInv . mixColumnsInv . keyAdd k

finalRoundDecrypt :: Key -> Block -> Block
finalRoundDecrypt k = byteSubInv . shiftRowsInv . keyAdd k
