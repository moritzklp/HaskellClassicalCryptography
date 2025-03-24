\section{One-Time Pad}\label{sec:OTP}

One-Time Pad (OTP) is a symmetric encryption based on the bitwise XOR (exclusive OR) operation. 
Given a plaintext message $m$ and a secret key $k$, the ciphertext $c$ is computed as:
\[ c = m \oplus k \]
where $\oplus$ denotes the bitwise XOR operation.
Decryption is achieved by performing the bitwise XOR on the ciphertext and the key, which results in the original plaintext.
\[ m = c \oplus k \]
To perform the bitwise XOR operations, the secret key must have the same length as the plaintext or ciphertext. 
The security of OTP relies on the key being truly random, never reused, and kept secret from any adversary.

In this section we implement the functionality of the One-Time Pad encryption and decryption. 
This implementation provides a command-line interface that allows users to generate keys, 
encrypt plaintext messages, and decrypt ciphertext back to the original text. 
The design emphasizes clarity and modularity, leveraging the helper functions from the \texttt{Pad} module.

\begin{code}
module OTP where

import System.IO
import System.Random (randomRs, newStdGen)
import Pad
import MTP
\end{code}

\subsection{Encryption and Decryption}\label{sec:otp_user_interaction}

Encryption and decryption is handled by the \texttt{encryptIO} and \texttt{decryptIO} functions, respectively.
These functions read the input and key files, perform the encryption or decryption, and write the result to the output file.
The actual bitwise XOR operations are performed by the \texttt{padString} function from the \texttt{Pad} module.

\begin{code}
-- | Encrypts a plaintext file using a key file and writes the result to an output file
encryptIO :: String -> String -> String -> IO ()
encryptIO outputFile inputFile keyFile = do
    inputContent <- readFile inputFile
    keyContent <- readFile keyFile
    let ciphertext = padString inputContent keyContent
    writeFile outputFile ciphertext

-- | Decrypts a ciphertext file using a key file and writes the result to an output file
decryptIO :: String -> String -> String -> IO ()
decryptIO outputFile inputFile keyFile = do
    inputContent <- readFile inputFile
    keyContent <- readFile keyFile
    let plaintext = padString inputContent keyContent
    writeFile outputFile plaintext
\end{code}

\subsection{Key Generation}\label{sec:otp_key_generation}

The key used to encrypt the plaintext must be as long as the plaintext itself.
Therefore the key generation functions ensure that the key length matches the length of the input plaintext.
The key is generated using random characters from the ASCII range \texttt{'!'} to \texttt{'~'}.

\begin{code}
-- | Generates a random key of a given length
generateRandomKeyIO :: Int -> IO String
generateRandomKeyIO n = take n . randomRs ('!', '~') <$> newStdGen

-- | Generates a key of the same length as the plaintext and writes it to a file
generateKeyFromPlaintextIO :: String -> String -> IO ()
generateKeyFromPlaintextIO inputFile keyFile = do
    inputContent <- readFile inputFile
    let n = length inputContent
    key <- generateRandomKeyIO n
    writeFile keyFile key
\end{code}


\subsection{User Interaction}\label{sec:otp_user_interaction}

The \texttt{main} function provides a command-line interface for the user to select the desired operation: 
key generation, encryption, decryption, or a demonstration of the Multi-Time Pad (MTP) attack.

\begin{code}
-- | Handles user interactions and operations selection
otp :: IO ()
otp = do
    hSetBuffering stdin LineBuffering
    putStrLn "Hello, do you want to generate a key, encrypt, decrypt or execute the Multi-Time Pad attack? (generate/encrypt/decrypt/mtp)"
    
    method <- getLine
    case method of
        "generate" -> handleGenerateKey
        "encrypt" -> handleEncrypt
        "decrypt" -> handleDecrypt
        "mtp" -> handleMTP
        _ -> putStrLn "Invalid method. Please choose 'generate', 'encrypt', 'decrypt', or 'mtp'."
\end{code}

If the user selects the key generation option, the program prompts for the filenames of the key and plaintext files.
The key is then generated and stored in the specified key file.
The plaintext file is used to determine the length of the key.

\begin{code}
-- | Handles key generation interaction
handleGenerateKey :: IO ()
handleGenerateKey = do
    putStrLn "In what file do you want to store the key? (e.g., key.txt)"
    keyFile <- getLine
    putStrLn "What plaintext do you want to generate a key for? (e.g., input.txt)"
    inputFile <- getLine
    generateKeyFromPlaintextIO inputFile keyFile
    putStrLn $ "Key generated and stored in " ++ keyFile
\end{code}

If the user selects the encryption or decryption option, the program prompts for the filenames of the input and output files, as well as the key file.
The encryption or decryption operation is then performed using the specified files.

\begin{code}
-- | Handles encryption interaction
handleEncrypt :: IO ()
handleEncrypt = do
    putStrLn "In what file do you want to store the ciphertext? (e.g., output.txt)"
    outputFile <- getLine
    putStrLn "What plaintext do you want to encrypt? (e.g., input.txt)"
    inputFile <- getLine
    putStrLn "What key do you want to use? (e.g., key.txt)"
    keyFile <- getLine
    encryptIO outputFile inputFile keyFile
    putStrLn $ "Plaintext encrypted and stored in " ++ outputFile

-- | Handles decryption interaction
handleDecrypt :: IO ()
handleDecrypt = do
    putStrLn "In what file do you want to store the plaintext? (e.g., output.txt)"
    outputFile <- getLine
    putStrLn "What ciphertext do you want to decrypt? (e.g., input.txt)"
    inputFile <- getLine
    putStrLn "What key do you want to use? (e.g., key.txt)"
    keyFile <- getLine
    decryptIO outputFile inputFile keyFile
    putStrLn $ "Ciphertext decrypted and stored in " ++ outputFile
\end{code}

If the user selects the MTP attack option, the program loads the ciphertexts from a file and performs the Many-Time Pad attack.
By default, the mtp.txt file contains the ciphertexts from the MTP challenge from the 
"Introduction to Modern Cryptography" course (https://homepages.cwi.nl/~schaffne/courses/crypto/2012/).

\begin{code}
-- | Handles Multi-Time Pad attack
handleMTP :: IO ()
handleMTP = do
    hexCiphertexts <- loadHexList "ciphertexts/mtp.txt"
    let ciphertexts = map hexToBytes hexCiphertexts
    mapM_ (breakIO ciphertexts) ciphertexts
    putStrLn "MTP attack completed"
\end{code}




