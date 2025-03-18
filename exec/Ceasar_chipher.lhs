\begin{code}
module Ceasar_chipher where

import System.IO
import System.Random (randomR, newStdGen)

caesarEncrypt :: Int -> String -> String
caesarEncrypt shift = map (shiftChar shift)
  where
    shiftChar s c =
      let base = fromEnum '!'
          offset = (fromEnum c - base + s) `mod` 94
      in toEnum (base + offset)

caesarDecrypt :: Int -> String -> String
caesarDecrypt shift = caesarEncrypt (-shift)

encryptIO :: String -> String -> String -> IO ()
encryptIO output inputFile keyFile = do
    inputContent <- readFile inputFile
    keyContent <- readFile keyFile
    let shift = read keyContent :: Int
    writeFile output (caesarEncrypt shift inputContent)

decryptIO :: String -> String -> String -> IO ()
decryptIO output inputFile keyFile = do
    inputContent <- readFile inputFile
    keyContent <- readFile keyFile
    let shift = read keyContent :: Int
    writeFile output (caesarDecrypt shift inputContent)

generateCaesarKeyIO :: IO String
generateCaesarKeyIO = do
    gen <- newStdGen
    let (shift, _) = randomR (1, 93 :: Int) gen  -- Annotate literal to Int
    return (show (shift :: Int))

generateKeyFromPlaintextIO :: String -> String -> IO ()
generateKeyFromPlaintextIO inputFile keyfile = do
    _ <- readFile inputFile  -- Read input but don't use length
    key <- generateCaesarKeyIO
    writeFile keyfile key

ceasar :: IO ()
ceasar = do
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