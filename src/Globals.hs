module Globals where

import Data.Word

type Block = [Word8]

type Key = [Word8]

type KeyWord = [Word8]

blockSize :: Num a => a
blockSize = 16

numRounds :: Num a => a
numRounds = 10

chunkSize :: Num a => a
chunkSize = 4

aesPolynomial :: Num a => a
aesPolynomial = 0x11B
