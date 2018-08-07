module EncryptDecrypt
    ( encrypt,
      decrypt,
    ) where

import Control.Monad.State.Lazy
import Data.Bits (xor)

import Globals (Key, Block, numRounds)
import SubKeyGen (generateSubKeys)
import Round (roundEncrypt, roundDecrypt)
import Round.Internal (keyAdd, byteSub, byteSubInv, shiftRows, shiftRowsInv)

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
