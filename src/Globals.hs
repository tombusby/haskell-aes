module Globals where

import Data.Word

type Block = [Word8]
type Key = [Word8]

blockSize :: Num a => a
blockSize = 16

numRounds :: Num a => a
numRounds = 10
