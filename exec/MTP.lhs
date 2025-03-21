\section{Many-Time Pad}

When the same key $k$ is reused for multiple messages $m_1, m_2$:
\begin{align*}
c_1 &= m_1 \oplus k \\
c_2 &= m_2 \oplus k
\end{align*}

An attacker can compute:
\[ c_1 \oplus c_2 = (m_1 \oplus k) \oplus (m_2 \oplus k) = m_1 \oplus m_2 \]

This eliminates the key and reveals the XOR of plaintexts. While $m_1 \oplus m_2$ isn't immediately readable, attackers can use frequency analysis and known plaintext patterns to recover both messages \cite{Denning83}. \\

For example: 

Consider two messages encrypted with the same key:
\begin{align*}
m_1 &= \texttt{"HelloWorld"} \\
m_2 &= \texttt{"SecureData"} \\
k &= \texttt{0x5f1d3a...} \quad\text{(random bytes)}
\end{align*}

The attacker observes:
\begin{align*}
c_1 &= m_1 \oplus k \\
c_2 &= m_2 \oplus k
\end{align*}

By computing $c_1 \oplus c_2$, the attacker gets $m_1 \oplus m_2$. If they guess part of $m_1$ (e.g., common phrase "Hello"), they can recover the corresponding part of $m_2$:
\[ \text{Guessed } m_1 \oplus (m_1 \oplus m_2) = m_2 \]

As previously stated, the One-Time Pad is secure and resistant to attacks. However, in case where not all the keys are unique, so a key is reused, it is possible to break the encyption. The method to do it is called a Many-Time Pad Attack.
The cipher becomes vulnerable because if two plaintexts have been ecypted with the same key. Performing the XOR operation on two cipher-texts encrypted with the same key, will have the same result as performing the XOR operations on the two original plaintexts. What it means is that we remove the secret key from the equation completely.

\begin{code}
module MTP where

import System.Environment (getArgs)
import Data.Char (chr, ord, isAscii)
import Data.Word (Word8)
import Data.Bits (xor)
import Data.List (intercalate, foldl')
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Text.Printf (printf)
import GHC.Base (Multiplicity(Many))
import Numeric.Lens (hex)
\end{code}

The ciphertexts are loaded from a file, and then the program iterates over each ciphertext, and performs the Many-Time Pad attack.

\begin{code}
-- Function to split a ByteString containing comma-separated hex values
splitHexStrings :: BS.ByteString -> [String]
splitHexStrings = map C8.unpack . C8.split ','

-- Function to read hex strings from a file and split them
loadHexList :: FilePath -> IO [String]
loadHexList filePath = do
    contents <- C8.readFile filePath
    return $ splitHexStrings (C8.filter (/= '\n') contents) -- Remove newlines if present

main :: IO ()
main = do
    hexciphertexts <- loadHexList "ciphertexts/mtp.txt"
    let ciphertexts = map hexToBytes hexciphertexts
    mapM_ (breakIO ciphertexts) ciphertexts

-- Process and decrypt a single ciphertext using information from all ciphertexts
breakIO :: [BS.ByteString] -> BS.ByteString -> IO ()
breakIO allCiphertexts targetCiphertext = do
    -- Make all ciphertexts the same length as the target ciphertext
    let normalizedCiphertexts = map (BS.take (BS.length targetCiphertext)) allCiphertexts

    -- Find space positions for all ciphertexts
    let ciphertextsWithSpaceInfo = analyzeAllCiphertexts normalizedCiphertexts

    -- Initialize empty key and update it with space information
    let emptyKey = replicate (fromIntegral $ BS.length targetCiphertext) Nothing
    let partialKey = createPartialKey emptyKey ciphertextsWithSpaceInfo

    -- Decrypt the target ciphertext
    putStrLn $ breakWithPartialKey (BS.unpack targetCiphertext) partialKey

\end{code}

To perform the attack, the hex strings are converted to ByteStrings, so that the XOR operation can be used on them.

\begin{code}

-- XOR two ByteStrings together
bytesXor :: BS.ByteString -> BS.ByteString -> BS.ByteString
bytesXor a b = BS.pack $ zipWith xor (BS.unpack a) (BS.unpack b)

-- Convert a hexadecimal string to a ByteString
hexToBytes :: String -> BS.ByteString
hexToBytes [] = BS.empty
hexToBytes (a:b:rest) = BS.cons (fromIntegral $ hexValue a * 16 + hexValue b) (hexToBytes rest)
    where
        hexValue :: Char -> Int
        hexValue c
            | c >= '0' && c <= '9' = ord c - ord '0'
            | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
            | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10
            | otherwise = error $ "Invalid hex character: " ++ [c]
hexToBytes _ = error "Invalid hex string: ciphertext must have even number of characters hex characters"

\end{code}

The Many-Time Pad attack uses the fact that a letter XOR-ed with a space returns a letter.
If two plaintexts are XOR-ed, and there is a letter in the result, one of the plaintext had a space in that position.
The following functions are used to analyze the ciphertexts and find likely space positions in each of them.

\begin{code}

-- Check if a byte is likely to be a space in plaintext (1 for space locations, 0 otherwise)
markAsSpace :: Word8 -> Int
markAsSpace byte | isLikelySpace byte = 1
                 | otherwise = 0
    where isLikelySpace b = (b >= 65 && b <= 90) || (b >= 97 && b <= 122) || b == 0

-- Find likely space positions for two ciphertexts
detectSpacePositions :: BS.ByteString -> BS.ByteString -> [Int]
detectSpacePositions ciphertext1 ciphertext2 =
    map (markAsSpace . BS.index (bytesXor ciphertext1 ciphertext2)) [0 .. BS.length ciphertext1 - 1]

-- Find likely space positions for all ciphertexts
-- A position is likely a space if it produces a letter when XORed with most other ciphertexts
findLikelySpaces :: BS.ByteString -> [BS.ByteString] -> [Int]
findLikelySpaces target otherCiphertexts =
    let initialCounts = replicate (BS.length target) 0
        spaceIndicators = map (detectSpacePositions target) otherCiphertexts
        voteCounts = foldr (zipWith (+)) initialCounts spaceIndicators
        threshold = length otherCiphertexts - 2
    in map (\count -> if count > threshold then 1 else 0) voteCounts

-- Perform the findLikelySpaces function on each ciphertext to find likely space positions in each of them
analyzeAllCiphertexts :: [BS.ByteString] -> [(BS.ByteString, [Int])]
analyzeAllCiphertexts ciphertexts =
    map (\cipher -> (cipher, findLikelySpaces cipher (filter (/= cipher) ciphertexts))) ciphertexts

\end{code}

This code block uses all the space position information to create a partial key. 
Only bytes of the key in positions where one of the original plaintext had a space are recovered. 

\begin{code}

-- Create the partial key from the space information
-- Apply the updatePartialKey to the key for each ciphertext
createPartialKey :: [Maybe Word8] -> [(BS.ByteString, [Int])] -> [Maybe Word8]
createPartialKey emptyKey ciphertextsWithSpaces = xorWithSpace (foldl updatePartialKey emptyKey ciphertextsWithSpaces)
    where
        xorWithSpace = map (fmap (\b -> b `xor` fromIntegral (ord ' ')))

-- Update the partial key with the space information from a single ciphertext
updatePartialKey ::  [Maybe Word8] -> (BS.ByteString, [Int]) -> [Maybe Word8]
updatePartialKey oldKey (cipherText, spaceIndicators) = zipWith updateKeyByte oldKey (zip (BS.unpack cipherText) spaceIndicators)
    where 
        updateKeyByte currentByte (ciphertextByte, isSpace) = case (currentByte, isSpace) of
            (Nothing, 1) -> Just ciphertextByte
            (Just existing, 1) | existing == ciphertextByte -> Just existing
            _ -> currentByte

\end{code}

This function decrypts the ciphertext using the partial key.

\begin{code}

-- Decrypt a ciphertext using the partial key
breakWithPartialKey :: [Word8] -> [Maybe Word8] -> String
breakWithPartialKey = zipWith decryptByte
  where
    decryptByte b Nothing = '.'
    decryptByte b (Just keyByte) =
        if b == keyByte
        then ' '
        else chr $ fromIntegral (b `xor` keyByte)

\end{code}
