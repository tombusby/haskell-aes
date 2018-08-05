module Round
    ( roundEncrypt,
    ) where

import Globals (Key, Block, blockSize, numRounds)
import Round.Internal

roundEncrypt :: Key -> Block -> Block
roundEncrypt = undefined

roundDecrypt :: Key -> Block -> Block
roundDecrypt = undefined
