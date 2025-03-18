
\section{Many Time Pad Attack}
As previously stated, the One Time Pad is secure and resistant to attacks. However, in case where not all the keys are unique, so a key is reused, it is possible to break the encyption. The method to do it is called a Many Time Pad Attack.
The cipher becomes vulnerable because if two plaintexts have been ecypted with the same key, performing the XOR operation on the cipher-texts will have the same result as doing it with the original plaintexts. What it means is that we remove the secret key from the equation completely. This is shown in the equation: [insert equation]


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


