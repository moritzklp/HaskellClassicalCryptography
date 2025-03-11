\section{Helper Functions}\label{sec:OTP}

In this section we define helper functions.

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

