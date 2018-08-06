module SubKeyGen
    ( generateSubKeys
    ) where

import Control.Monad.State.Lazy
import Data.Word
import Data.Bits (xor)
import Data.List.Split (chunksOf)
import Crypto.Number.F2m (modF2m)

import Globals (Key, numRounds, aesPolynomial)
import SBox(sBox)

type KeyWord = [Word8]
type RC = Word8

generateSubKeys :: Key -> [Key]
generateSubKeys k = reverse keys
    where
        keys = snd . execState (keyGen numRounds k) $ (rcVals, [])

pushSubkey :: Key -> State ([RC], [Key]) ()
pushSubkey k = do
    (rcs, sks) <- get
    put (rcs, k:sks)

popRC :: State ([RC], [Key]) RC
popRC = do
    (rc:rcs, sks) <- get
    put (rcs, sks)
    return rc

keyGen :: Int -> Key -> State ([RC], [Key]) ()
keyGen 0 _ = return ()
keyGen n k = do
    rc <- popRC
    w0' <- return $ w0 `mapXor` g rc w3
    w1' <- return $ w0' `mapXor` w1
    w2' <- return $ w1' `mapXor` w2
    w3' <- return $ w2' `mapXor` w3
    pushSubkey . concat $ [w0', w1', w2', w3']
    keyGen (n-1) . concat $ [w0', w1', w2', w3']
        where
            mapXor = zipWith xor
            [w0, w1, w2, w3] = chunksOf 4 k

rcVals :: [RC]
rcVals = map (fromIntegral . modF2m aesPolynomial) (take numRounds $ doubles 1)
    where
        doubles n = n:(doubles $ 2 * n)

g :: RC -> KeyWord -> KeyWord
g rc kw = rcXor . map sBox $ kwRot
    where
        kwRot = let (l, r) = splitAt 1 kw in r ++ l
        rcXor (x:xs) = (rc `xor` x):xs
