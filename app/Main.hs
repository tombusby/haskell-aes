module Main where

import System.Environment (getProgName, getArgs)
import System.Directory (doesFileExist)

import EncryptDecrypt (encrypt, decrypt)
import Padding (pad, unpad)

checkArgs :: [String] -> IO ()
checkArgs [input, output, key] = do
    inputReal <- doesFileExist input
    let inputError = if inputReal then [] else input ++ " does not exist.\n"
    let errors = inputError
    putStr $ "Errors:\n\n" ++ inputError

main :: IO ()
main = do
    progName <- getProgName
    args <- getArgs
    if length args /= 3 then 
        putStrLn $ "Usage: " ++ progName ++ " <INPUT FILENAME> <OUTPUT FILENAME> <KEY>"
     else do
        checkArgs args
        
