module EncryptDecrypt
    ( encrypt,
      decrypt,
      encryptBlocksECB,
      decryptBlocksECB,
      encryptBlocksCBC,
      decryptBlocksCBC,
    ) where

import Control.Monad.State.Lazy
import Data.Bits (xor)

import Globals (Key, Block, numRounds)
import SubKeyGen (generateSubKeys)
import Round (roundEncrypt, roundDecrypt)
import Round.Internal (keyAdd, byteSub, byteSubInv, shiftRows, shiftRowsInv)
import Padding (pad, unpad)

type IV = Block

encryptBlocksECB :: Key -> [Block] -> [Block]
encryptBlocksECB key = map (encrypt key) . pad

decryptBlocksECB :: Key -> [Block] -> Maybe [Block]
decryptBlocksECB key = unpad . map (decrypt key)

encryptBlocksCBC :: Key -> IV -> [Block] -> [Block]
encryptBlocksCBC key iv = encryptBlocksCBC' key iv . pad

encryptBlocksCBC' :: Key -> IV -> [Block] -> [Block]
encryptBlocksCBC' _ _ [] = []
encryptBlocksCBC' key iv (b:bs) = c1 : encryptCBCAccumulate c1 bs
    where
        cbcEncrypt addVal = encrypt key . zipWith xor addVal
        c1 = cbcEncrypt iv b
        encryptCBCAccumulate _ [] = []
        encryptCBCAccumulate c (b:bs) = let cNext = cbcEncrypt c b in
            cNext : encryptCBCAccumulate cNext bs

decryptBlocksCBC :: Key -> IV -> [Block] -> Maybe [Block]
decryptBlocksCBC key iv = unpad . decryptBlocksCBC' key iv

decryptBlocksCBC' :: Key -> IV -> [Block] -> [Block]
decryptBlocksCBC' _ _ [] = []
decryptBlocksCBC' key iv (b:bs) = p1 : decryptCBCAccumulate b bs
    where
        cbcDecrypt addVal = zipWith xor addVal . decrypt key
        p1 = cbcDecrypt iv b
        decryptCBCAccumulate _ [] = []
        decryptCBCAccumulate c (b:bs) = let pNext = cbcDecrypt c b in
            pNext : decryptCBCAccumulate b bs

encrypt :: Key -> Block -> Block
encrypt key block = evalState state $ generateSubKeys key
    where
        state = encryptStateful numRounds $ keyAdd block key 

decrypt :: Key -> Block -> Block
decrypt key block = keyAdd key . evalState state $ reversedSubKeys
    where
        state = decryptStateful numRounds block
        reversedSubKeys = reverse $ generateSubKeys key

popSubKey :: State [Key] Key
popSubKey = do
    (subKey:ks) <- get
    put ks
    return subKey

encryptStateful :: Int -> Block -> State [Key] Block
encryptStateful 1 block = do
    subKey <- popSubKey
    return $ keyAdd subKey . shiftRows . byteSub $ block
encryptStateful n block = do
    subKey <- popSubKey
    encryptStateful (n-1) $ roundEncrypt subKey block

decryptStateful :: Int -> Block -> State [Key] Block
decryptStateful 0 block = return block
decryptStateful n block 
    | n == numRounds = do
        subKey <- popSubKey
        let finalRoundInv = byteSubInv . shiftRowsInv . keyAdd subKey
        decryptStateful (n-1) $ finalRoundInv block
    | n < numRounds = do
        subKey <- popSubKey
        decryptStateful (n-1) $ roundDecrypt subKey block
