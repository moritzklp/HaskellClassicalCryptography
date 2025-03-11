\section{One Time Pad}\label{sec:OTP}

In this section we implement the one time pad.

\begin{code}
module Main where

import System.Environment (getArgs)
import System.Random (randomRs, newStdGen)
import Pad

-- Should generate a key of length n and write it to a file
generateKeyIO :: String -> Int -> IO ()
generateKeyIO keyfile n = do
    key <- generateRandomKeyIO n
    writeFile keyfile key

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
generateRandomKeyIO n = take n . randomRs ('A', 'Z') <$> newStdGen

-- Given a plaintext, should generate a key of the same length
generateKeyFromPlaintextIO :: String -> String -> IO ()
generateKeyFromPlaintextIO inputFile keyfile = do
    inputContent <- readFile inputFile
    let n = length inputContent
    key <- generateRandomKeyIO n
    writeFile keyfile key

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("generate":inputFile:keyFile:_) -> generateKeyFromPlaintextIO inputFile keyFile
        ("encrypt":outputFile:inputFile:keyFile:_) -> encryptIO outputFile inputFile keyFile
        ("decrypt":outputFile:inputFile:keyFile:_) -> decryptIO outputFile inputFile keyFile
        _ -> putStrLn "Invalid arguments. Usage:\n\
                      \  generate [input-file.txt] [key-file.txt]\n\
                      \  encrypt [output-file.txt] [input-file.txt] [key-file.txt]\n\
                      \  decrypt [output-file.txt] [input-file.txt] [key-file.txt]"

\end{code}

We can run this program with the commands:

\begin{verbatim}
stack build
stack exec myprogram
\end{verbatim}

The output of the program is something like this:

\begin{verbatim}
Hello!
[1,2,3,4,5,6,7,8,9,10]
[100,100,300,300,500,500,700,700,900,900]
[1,3,0,1,1,2,8,0,6,4]
[100,300,42,100,100,100,700,42,500,300]
GoodBye
\end{verbatim}
