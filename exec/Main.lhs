
\section{Wrapping it up in an exectuable}\label{sec:Main}

We will now use the library form Section \ref{sec:Basics} in a program.

\begin{code}
module Main where

import OTP
import VigenereCipher
import CaesarCipher

main :: IO ()
main = do
  putStrLn "Hello!"
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
  putStrLn "GoodBye"
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
