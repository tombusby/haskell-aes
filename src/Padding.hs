module Padding
    ( pad
    , unpad
    ) where

import Globals (Block, blockSize)

generatePadding :: Int -> Block
generatePadding n = 0x80 : replicate (repNum n) 0x00
    where
        repNum n = blockSize - 1 - n

pad :: [Block] -> [Block]
pad [] = [generatePadding 0]
pad bs = init bs ++ (padded . last $ bs)
    where
        padded b
            | length b == blockSize = [b] ++ [(generatePadding 0)]
            | otherwise = [b ++ (generatePadding . length) b]

unpad :: [Block] -> [Block]
unpad = undefined
