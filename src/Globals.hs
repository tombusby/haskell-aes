module Globals where

import Data.Word

type Block = [Word8]

blockSize :: Num a => a
blockSize = 16
