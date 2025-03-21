\begin{code}
module Vigenere_cipher where

import System.IO
import System.Random (randomRs, newStdGen)

shiftChar :: Int -> Char -> Char
shiftChar s c =
  let base = fromEnum '!'
      offset = (fromEnum c - base + s) `mod` 94
  in toEnum (base + offset)

vigenereEncrypt :: String -> String -> String
vigenereEncrypt key plaintext = zipWith encryptChar (cycle key) plaintext
  where
    encryptChar k c = shiftChar (fromEnum k - fromEnum '!') c

vigenereDecrypt :: String -> String -> String
vigenereDecrypt key ciphertext = zipWith decryptChar (cycle key) ciphertext
  where
    decryptChar k c = shiftChar (-(fromEnum k - fromEnum '!')) c

encryptIO :: String -> String -> String -> IO ()
encryptIO output inputFile keyFile = do
    inputContent <- readFile inputFile
    keyContent <- readFile keyFile
    writeFile output (vigenereEncrypt keyContent inputContent)

decryptIO :: String -> String -> String -> IO ()
decryptIO output inputFile keyFile = do
    inputContent <- readFile inputFile
    keyContent <- readFile keyFile
    writeFile output (vigenereDecrypt keyContent inputContent)

generateRandomKeyIO :: Int -> IO String
generateRandomKeyIO n = take n . randomRs ('!', '~') <$> newStdGen

generateKeyFromPlaintextIO :: String -> String -> IO ()
generateKeyFromPlaintextIO inputFile keyfile = do
    inputContent <- readFile inputFile
    let n = length inputContent
    key <- generateRandomKeyIO n
    writeFile keyfile key

vig :: IO ()
vig = do
    hSetBuffering stdin LineBuffering
    putStrLn "Hello, do you want to generate a key, encrypt, or decrypt? (generate/encrypt/decrypt)"
    method <- getLine
    case method of
        "generate" -> do
            putStrLn "In what file do you want to store the key? (e.g., key.txt)"
            keyFile <- getLine
            putStrLn "What plaintext do you want to generate a key for? (e.g., input.txt)"
            inputFile <- getLine
            generateKeyFromPlaintextIO inputFile keyFile
        "encrypt" -> do
            putStrLn "In what file do you want to store the ciphertext? (e.g., output.txt)"
            outputFile <- getLine
            putStrLn "What plaintext do you want to encrypt? (e.g., input.txt)"
            inputFile <- getLine
            putStrLn "What key do you want to use? (e.g., key.txt)"
            keyFile <- getLine
            encryptIO outputFile inputFile keyFile
        "decrypt" -> do
            putStrLn "In what file do you want to store the plaintext? (e.g., output.txt)"
            outputFile <- getLine
            putStrLn "What ciphertext do you want to decrypt? (e.g., input.txt)"
            inputFile <- getLine
            putStrLn "What key do you want to use? (e.g., key.txt)"
            keyFile <- getLine
            decryptIO outputFile inputFile keyFile
        _ -> putStrLn "Invalid method. Please choose 'generate', 'encrypt', or 'decrypt'."
\end{code}