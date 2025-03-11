\section{Helper Functions}\label{sec:helper-functions}

In this section we define helper functions that are essential for our implementation of the OTP cipher. The code below shows our Haskell implementation, which includes a function to perform an XOR operation on two byte strings. This functionality is a key component in both the encryption process and in demonstrating the Many-Time Pad attack.

\begin{itemize}
    \item \textbf{Module and Imports:} The module \texttt{Pad} is defined and exports the \texttt{padString} function. It imports libraries from \texttt{Data.ByteString} and \texttt{Data.ByteString.Char8} for efficient handling of binary and character data, and \texttt{Data.Bits} for bitwise operations.
    \item \textbf{The \texttt{xorBytes} Function:} This function takes two \texttt{ByteString} arguments and applies a pair-wise XOR operation using \texttt{B.zipWith xor}. The result is packed back into a \texttt{ByteString} using \texttt{B.pack}. This operation is key in combining the plaintext with the key in an OTP cipher.
    \item \textbf{The \texttt{padString} Function:} This exported function takes two strings, converts them into \texttt{ByteString}s, and then applies the \texttt{xorBytes} function. Finally, it converts the result back into a string. This process effectively “pads” one string with another using the XOR operation, a fundamental step in many cryptographic techniques.
    \item \textbf{Demonstration:} The \texttt{main} function provides an example of how \texttt{xorBytes} can be applied to two hard-coded byte arrays. This sample code illustrates the practical use of the XOR operation.
\end{itemize}

\begin{code}
module Pad (padString) where

-- :set -package bytestring
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
--import Data.Word (Word8)
import Data.Bits (xor)

-- xor :: Bits a => a -> a -> a
xorBytes :: B.ByteString -> B.ByteString -> B.ByteString
xorBytes bs1 bs2 = B.pack (B.zipWith xor bs1 bs2)

padString :: String -> String -> String
padString s1 s2 = C.unpack $ xorBytes (C.pack s1) (C.pack s2)

-- main :: IO ()
-- main = do
--     let bytes1 = B.pack [0x01, 0x02, 0x03, 0x04]  -- First byte array
--     let bytes2 = B.pack [0xFF, 0x00, 0xFF, 0x00]  -- Second byte array
--     let result = xorBytes bytes1 bytes2           -- Perform XOR operation
--     print result  -- Output: "\254\STX\252\DLE"
\end{code}

