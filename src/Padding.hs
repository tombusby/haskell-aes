module Padding
    ( pad
    , unpad
    ) where

import Globals (Block, blockSize)

generatePadding :: Int -> Block
generatePadding n = 0x80 : replicate (blockSize - 1 - n) 0x00

pad :: [Block] -> [Block]
pad [] = [generatePadding 0]
pad bs = init bs ++ (padded . last $ bs)
    where
        padded b
            | length b == blockSize = [b] ++ [(generatePadding 0)]
            | otherwise = [b ++ (generatePadding . length) b]

unpad :: [Block] -> Maybe [Block]
unpad [] = Nothing
unpad bs
    | last bs == generatePadding 0 = Just $ init bs
    | otherwise = (init bs ++) <$> (:[]) <$> (stripPadding . last) bs

stripPadding :: Block -> Maybe Block
stripPadding b = let b' = dropWhile (==0) . reverse $ b in
    case b' of
        [] -> Nothing
        (0x80:ws) -> Just $ reverse ws
        otherwise -> Nothing
