module Main where

import qualified Data.ByteString as B
import Data.Char (toUpper)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import EncryptDecrypt (decryptBlocksECB, encryptBlocksECB)
import Globals (Key, blockSize)
import Numeric (readHex)
import System.Directory (doesFileExist)
import System.Environment (getArgs, getProgName)

type Op = String

type Input = String

type Output = String

type KeyString = String

doOperation :: Op -> Input -> Output -> Key -> IO ()
doOperation "encrypt" input output key = do
  putStrLn "Reading input..."
  plaintextBlocks <- chunksOf blockSize . B.unpack <$> B.readFile input
  putStrLn "Encryping input..."
  let ciphertextBlocks = encryptBlocksECB key plaintextBlocks
  putStrLn "Writing to output file..."
  B.writeFile output . B.pack . concat $ ciphertextBlocks
doOperation "decrypt" input output key = do
  putStrLn "Reading input..."
  ciphertextBlocks <- chunksOf blockSize . B.unpack <$> B.readFile input
  putStrLn "Decryping input..."
  case decryptBlocksECB key ciphertextBlocks of
    Just blocks -> do
      putStrLn "Writing to output file..."
      B.writeFile output . B.pack . concat $ blocks
    Nothing ->
      putStrLn "Invalid data: unpad failed."
doOperation _ _ _ _ = return ()

keyStringToKey :: KeyString -> Maybe Key
keyStringToKey ks
  | length ks /= 32 = Nothing
  | allValidChars ks = Just key
  | otherwise = Nothing
  where
    key = map fst . concatMap readHex . chunksOf 2 $ ks
    allValidChars ks = all ((`elem` "0123456789ABCDEF") . toUpper) ks

parseKeyString :: KeyString -> IO (String, Maybe Key)
parseKeyString ks = case keyStringToKey ks of
  Nothing -> return ("Key is not a valid 128bit hex number\n", Nothing)
  k -> return ([], k)

checkArgs :: [String] -> IO ()
checkArgs [op, input, output, key] = do
  let opError = if op `elem` validOps then [] else opErrorMessage
  inputReal <- doesFileExist input
  let fileError = if inputReal then [] else input ++ " does not exist.\n"
  (keyError, parsedKey) <- parseKeyString key
  let errors = opError ++ fileError ++ keyError
  if not (null errors)
    then putStr $ "Errors:\n\n" ++ errors
    else doOperation op input output $ fromJust parsedKey
  where
    validOps = ["encrypt", "decrypt"]
    opErrorMessage = "Operation must be either encrypt or decrypt\n"
checkArgs _ = return ()

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  if length args /= 4
    then
      putStrLn $
        "Usage: " ++ progName
          ++ "<encrypt/decrypt> <INPUT FILENAME> <OUTPUT FILENAME> <KEY>"
    else checkArgs args
