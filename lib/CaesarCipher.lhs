\begin{code}
module CaesarCipher where

import System.IO
import System.Random (randomR, newStdGen)
import Data.Char (isAlpha, toUpper, toLower)
import Frequency (findBestShift)

caesarEncrypt :: Int -> String -> String
caesarEncrypt shift = map (shiftChar shift)
  where
    shiftChar s c
      | isAlpha c = let upperC = toUpper c
                        base = fromEnum 'A'
                        offset = (fromEnum upperC - base + s) `mod` 26
                    in toEnum (base + offset)
      | otherwise = c

caesarDecrypt :: Int -> String -> String
caesarDecrypt shift = caesarEncrypt (-shift)

encryptIO :: String -> String -> String -> IO ()
encryptIO output inputFile keyFile = do
    inputContent <- readFile inputFile
    keyContent <- readFile keyFile
    let shift = read keyContent :: Int
    writeFile output (caesarEncrypt shift (map toUpper inputContent))

decryptIO :: String -> String -> String -> IO ()
decryptIO output inputFile keyFile = do
    inputContent <- readFile inputFile
    keyContent <- readFile keyFile
    let shift = read keyContent :: Int
    writeFile output (caesarDecrypt shift (map toUpper inputContent))

generateCaesarKeyIO :: IO String
generateCaesarKeyIO = do
    gen <- newStdGen
    let (shift, _) = randomR (1, 25 :: Int) gen
    return (show (shift :: Int))

generateKeyFromPlaintextIO :: String -> String -> IO ()
generateKeyFromPlaintextIO inputFile keyfile = do
    _ <- readFile inputFile
    key <- generateCaesarKeyIO
    writeFile keyfile key

crackIO :: String -> String -> IO ()
crackIO output inputFile = do
    ciphertext <- readFile inputFile
    let bestShift = findBestShift (map toLower ciphertext)
        decrypted = caesarDecrypt bestShift (map toUpper ciphertext)
    writeFile output decrypted
    putStrLn $ "Guessed shift: " ++ show bestShift

caesar :: IO ()
caesar = do
    hSetBuffering stdin LineBuffering
    putStrLn "[Caesar] Do you want to generate a key, encrypt, decrypt, or crack? (generate/encrypt/decrypt/crack)"
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
        "crack" -> do
            putStrLn "In what file do you want to store the decrypted text? (e.g., output.txt)"
            outputFile <- getLine
            putStrLn "What ciphertext do you want to crack? (e.g., input.txt)"
            inputFile <- getLine
            crackIO outputFile inputFile
        _ -> putStrLn "Invalid method. Please choose 'generate', 'encrypt', 'decrypt', or 'crack'."
\end{code}
