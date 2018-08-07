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
        state = encryptSateful numRounds $ keyAdd block key 

decrypt :: Key -> Block -> Block
decrypt = undefined

popSubKey :: State ([Key]) Key
popSubKey = do
    (subKey:ks) <- get
    put ks
    return subKey

encryptSateful :: Int -> Block -> State ([Key]) Block
encryptSateful 1 block = do
    subKey <- popSubKey
    return $ keyAdd subKey . shiftRows . byteSub $ block
encryptSateful n block = do
    subKey <- popSubKey
    encryptSateful (n-1) $ roundEncrypt subKey block
