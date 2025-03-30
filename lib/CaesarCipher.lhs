\section{Caesar Cipher}
A Caesar cipher is a monoalphabetic substitution cipher, 
where each letter in the plaintext is replaced by a letter a fixed number of positions down the alphabet. 
For instance, with a shift of 3, the letter A is replaced by D, B by E, and so on. 
Despite its simplicity and vulnerability to brute force attacks, the Caesar cipher is a fundamental example in the study of cryptography.

The Caesar cipher module provides functionality for encryption, decryption, generating keys, and cracking Caesar ciphers.

\subsection*{Module Declaration and Imports}
The module starts by declaring its name and importing necessary libraries:
\begin{itemize}
    \item \texttt{System.IO} for input/output operations.
    \item \texttt{System.Random} for generating random numbers (used in key generation).
    \item \texttt{Data.Char} for character manipulations (e.g., converting characters to uppercase).
    \item Our module \texttt{Frequency} which provides a \texttt{findBestShift} function for cracking the cipher.
\end{itemize}

\begin{code}
module CaesarCipher where

import System.IO
import System.Random (randomR, newStdGen)
import Data.Char (isAlpha, toUpper, toLower)
import Frequency (findBestShift)
\end{code}

\subsection*{Encryption and Decryption Functions}
The \texttt{caesarEncrypt} function takes an integer shift and a string, applying the shift to each alphabetical character. It:
\begin{itemize}
    \item Converts the character to uppercase.
    \item Computes its offset from the base character 'A'.
    \item Applies the shift modulo 26 to wrap around the alphabet.
    \item Converts the result back to a character.
\end{itemize}
Non-alphabetical characters are returned unchanged.

The \texttt{caesarDecrypt} function leverages \texttt{caesarEncrypt} by using the negative shift, effectively reversing the encryption process.

\begin{code}
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
\end{code}

\subsection*{I/O Functions for Encryption and Decryption}
The functions \texttt{encryptIO} and \texttt{decryptIO} handle file operations:
\begin{itemize}
    \item They read the input text and key from files.
    \item Convert the key from a string to an integer.
    \item Convert the input text to uppercase before processing to ensure consistency.
    \item Write the encrypted or decrypted result back to an output file.
\end{itemize}

\begin{code}
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
\end{code}

\subsection*{Key Generation Functions}
The function \texttt{generateCaesarKeyIO} generates a random shift between 1 and 25 using a random number generator. 
The function \texttt{generateKeyFromPlaintextIO} then uses this key to create a key file for a given plaintext file, 
even though the plaintext is not directly used in generating the key.

\begin{code}
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
\end{code}

\subsection*{Cracking the Cipher}
The \texttt{crackIO} function attempts to decrypt a ciphertext without a known key by:
\begin{itemize}
    \item Reading the ciphertext from a file.
    \item Using the \texttt{findBestShift} function (from the \texttt{Frequency} module) to estimate the shift based on frequency analysis.
    \item Decrypting the ciphertext with the guessed shift.
    \item Writing the result to an output file and displaying the guessed shift.
\end{itemize}

\begin{code}
crackIO :: String -> String -> IO ()
crackIO output inputFile = do
    ciphertext <- readFile inputFile
    let bestShift = findBestShift (map toLower ciphertext)
        decrypted = caesarDecrypt bestShift (map toUpper ciphertext)
    writeFile output decrypted
    putStrLn $ "Ciphertext: " ++ show ciphertext
    putStrLn $ "Guessed shift: " ++ show bestShift
    putStrLn $ "Guessed text: " ++ show decrypted
\end{code}

\subsection*{User Interface}
The \texttt{caesar} function provides a simple command-line interface that promts the user to choose an action (generate, encrypt, decrypt, or crack) 
and then reads the necessary file names. It then calls the appropriate function based on the user's input.

\begin{code}
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
