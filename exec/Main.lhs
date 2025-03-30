
\section{Wrapping it up in an exectuable}\label{sec:Main}

The libraries with implementations for the OTP, Vigenere and Caesar ciphers are all imported into the main module.
The main module is the entry point of the program. It provides a simple command-line interface for the user to choose between the three ciphers.
The user is prompted to select a cipher method (OTP, Vigenere, or Caesar) and the corresponding function is called based on the user's input.

\begin{code}
module Main where

import OTP
import VigenereCipher
import CaesarCipher

main :: IO ()
main = do
  putStrLn "Do you want to do One Time Pad (OTP), Vigenere Cipher or Caesar Cipher? (o/v/c)"
  method <- getLine
  case method of
    "o" -> do
      otp
    "v" -> do
      vig
    "c" -> do
      caesar
    _ -> putStrLn "Invalid method"
\end{code}
