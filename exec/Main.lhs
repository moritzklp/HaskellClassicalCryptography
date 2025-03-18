\section{One Time Pad}\label{sec:OTP}

In this section we implement the main functionality of the One-Time Pad cipher. This implementation provides a command-line interface that allows users to generate keys, encrypt plaintext messages, and decrypt ciphertext back to the original text. The design emphasizes clarity and modularity, leveraging the helper functions from the \texttt{Pad} module.

Key aspects of the implementation include:
\begin{itemize}
    \item \textbf{Key Generation:} Two approaches are provided. One function generates a random key of a specified length, while another automatically generates a key that exactly matches the length of the input plaintext.
    \item \textbf{Encryption and Decryption:} Both operations use the same \texttt{padString} function to perform a bitwise XOR between the message and the key. This ensures that encryption and decryption are symmetric, as applying the XOR operation twice with the same key returns the original message.
    \item \textbf{Command-Line Interface:} The \texttt{main} function parses command-line arguments to determine whether to generate a key, encrypt a message, or decrypt a message. Clear usage instructions are provided for cases when the arguments do not match any of the expected patterns.
    \item \textbf{Random Key Generation:} By using Haskell's random number generator, the program creates a key consisting of uppercase letters, ensuring that each key is unpredictable and secure when used only once.
\end{itemize}

\subsection{Why Haskell}\label{sec:why_haskell}

\paragraph{Compiled and Optimized Execution} Studies have shown that Haskell implementations of cryptographic functions can perform nearly as well as C. 
For example, a Haskell implementation of the CAST-128 cipher ran within the same order of magnitude as an equivalent C version. 
This proves that it can handle computationally intensive cryptographic tasks.

\paragraph{Lazy Evaluation} Haskell uses lazy evaluation, which means that it only computes the values when they are needed.
This can help to improve the efficiency by avoiding unnecessary calculations. 
For our many-time pad attack this would help to handle large ciphertexts efficiently by processing them only when required.
This also helps to reduce the memory usage and computation overhead.

\paragraph{Memory Safety}
Unlike languages like C, Haskell automatically manages the memory, preventing vulnerabilities such as buffer overflows and pointer-related bugs.
This is important in cryptographic applications because small memory errors can lead to security flaws. 
With Haskell we do not have to worry about memory corruption or unexpected behavior due to false memory management.

\paragraph{Strong Type System}
Haskell has a strong type system that ensures that variables hold only correct kinds of values. 
This reduces the programming mistakes such as mixing up data types or performing incorrect calculations on data.
The type system also prevents unintended operations, such as treating a byte array as a string, which could introduce security risks in cryptographic applications.

\paragraph{Immutability}
Haskell's immutability (by default) means that once a value is created, it cannot be changed. 
This prevents unintended modifications of the data during the execution, which can be a problem in other languages where variables can be overwritten accidentally.

\paragraph{Arbitrary Precision Arithmetic}
Haskell allows computations with arbitrary large numbers which prevents the overflow issues that are common in many other languages.
This is useful in cryptographic application where calculations may involve large integers, and unexpected overflows could lead to incorrect results.

\paragraph{Purity}
Haskell's pure functions make sure that the same input always produces the same output, which makes cryptographic computations easier to test and debug. 
Since there are no hidden side effects, cryptographic functions can be verified more easily than in imperative languages.


\subsection{Code}\label{sec:code}
The following code block contains the complete implementation of the OTP functionality:

\begin{code}
module Main where

import System.IO
import System.Random (randomRs, newStdGen)
import Pad

-- Should read plaintext from input file and key from key file
-- Should write ciphertext to output file
encryptIO :: String -> String -> String -> IO ()
encryptIO output inputFile keyFile = do
    inputContent <- readFile inputFile
    keyContent <- readFile keyFile
    let ciphertext = padString inputContent keyContent
    writeFile output ciphertext

-- Should read ciphertext from input file and key from key file
-- Should write plaintext to output file
decryptIO :: String -> String -> String -> IO ()
decryptIO output inputFile keyFile = do
    inputContent <- readFile inputFile
    keyContent <- readFile keyFile
    let plaintext = padString inputContent keyContent
    writeFile output plaintext

-- Generate a random key of a given length (inside IO) - to not provide a seed manually
generateRandomKeyIO :: Int -> IO String
generateRandomKeyIO n = take n . randomRs ('!', '~') <$> newStdGen

-- Given a plaintext, should generate a key of the same length
generateKeyFromPlaintextIO :: String -> String -> IO ()
generateKeyFromPlaintextIO inputFile keyfile = do
    inputContent <- readFile inputFile
    let n = length inputContent
    key <- generateRandomKeyIO n
    writeFile keyfile key

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering -- So we can use backspace while running this using ghci
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




