\section{Multi Time Pad}

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



\begin{code}
module MTP where

import Data.Bits (xor)
import Data.Char (chr, ord)
import Data.List (transpose, maximumBy, isSuffixOf)
import Data.Ord (comparing)
import System.IO
import System.Directory (listDirectory)

mtp :: IO ()
mtp = do
    hSetBuffering stdin LineBuffering -- So we can use backspace while running this using ghci
    putStrLn "Please enter the folder name containing ciphertext files:"
    folder <- getLine
    files <- listDirectory folder
    let txtFiles = [folder ++ "/" ++ f | f <- files, ".txt" `isSuffixOf` f]
    if null txtFiles
        then putStrLn "No .txt files found in the specified folder."
        else do
            ciphertexts <- mapM readFile txtFiles
            let minLen = minimum (map length ciphertexts)
                truncated = map (take minLen) ciphertexts
                cipherAscii = map (map ord) truncated
                columns = transpose cipherAscii
                key = map guessKeyByte columns
                decrypted = map (\ct -> zipWith xor ct key) cipherAscii
            putStrLn "Recovered plaintexts:"
            mapM_ (putStrLn . map chr) decrypted

guessKeyByte :: [Int] -> Int
guessKeyByte column =
    let possibleCs = filter (\c -> c >= 97 && c <= 122) column  -- a-z
        candidates :: [(Int, Int)]  -- Explicit type annotation
        candidates = [ (keyCandidate, score)
                     | c <- possibleCs
                     , let keyCandidate = c `xor` 32  -- 32 is ASCII for space
                     , keyCandidate >= 65 && keyCandidate <= 90  -- Key must be A-Z
                     , let decrypted = map (`xor` keyCandidate) column
                     , let score = sum (map scoreChar decrypted) :: Int  -- Explicit type annotation
                     ]
        scoreChar :: Int -> Int  -- Explicit type for clarity
        scoreChar x
            | x == 32                      = 10  -- Space
            | x >= 97 && x <= 122          = 5   -- Lowercase
            | x >= 65 && x <= 90           = 5   -- Uppercase
            | x >= 48 && x <= 57           = 3   -- Digits
            | x `elem` punctuation         = 2   -- Punctuation
            | otherwise                    = 0
        punctuation = concat [ [33..47], [58..64], [91..96], [123..126] ]
    in if null candidates
        then 0  -- Default to 0 if no valid candidates
        else fst $ maximumBy (comparing snd) candidates

\end{code}