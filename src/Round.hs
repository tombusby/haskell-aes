module Round
    ( roundEncrypt,
    ) where

import Globals (Key, Block, blockSize, numRounds)

roundEncrypt :: Key -> Block -> Block
roundEncrypt = undefined

roundDecrypt :: Key -> Block -> Block
roundDecrypt = undefined
