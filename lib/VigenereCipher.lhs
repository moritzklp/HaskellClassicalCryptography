\section{Vigenère Cipher}
The Vigenère cipher is a polyalphabetic substitution cipher that encrypts alphabetic text by using a sequence of Caesar ciphers based on the letters of a keyword. Each letter of the key determines a shift for the corresponding character in the plaintext. Unlike the simple Caesar cipher, which uses one fixed shift, the Vigenère cipher employs multiple shifts determined by the key, making it more resilient against basic frequency analysis. However, methods like the Kasiski examination and the Friedman test can still be used to analyze and break it.

\subsection{Theory Behind the Vigenère Cipher}
In the Vigenère cipher:
\begin{itemize}
    \item A key (a string of letters) is used to determine a series of shifts. Each letter in the key corresponds to a shift value (e.g., A $\to$ 0, B $\to$ 1, ..., Z $\to$ 25).
    \item The plaintext is first cleaned (usually by removing non-alphabetic characters and converting to a common case) and then encrypted by shifting each letter according to the corresponding key letter, repeating (cycling) the key as needed.
    \item Decryption involves reversing the process by shifting the ciphertext in the opposite direction.
\end{itemize}
The strength of the cipher lies in the length and randomness of the key. Short or repetitive keys are vulnerable to statistical attacks such as the Kasiski examination, which looks for repeated sequences in the ciphertext, and the Friedman test, which uses the index of coincidence to estimate the key length.

\subsection{Overview of the Implementation}
This Haskell module implements the Vigenère cipher and includes:
\begin{itemize}
    \item Functions for encrypting and decrypting text using a given key.
    \item Utility functions for character manipulation and shifting.
    \item Techniques to analyze ciphertext, such as finding repeated sequences and calculating distances, which aid in estimating the key length.
    \item Frequency analysis to guess the key by examining individual columns of ciphertext.
    \item I/O operations to handle file input and output for encryption, decryption, key generation, and cipher cracking.
\end{itemize}

\subsection{Code Explanation}
Below is the complete code with detailed comments.

\subsubsection{Module Declaration and Imports}
The module is named \texttt{VigenereCipher} and imports several libraries:
\begin{itemize}
    \item \texttt{System.IO} for file I/O.
    \item List and character manipulation libraries for processing the text.
    \item Random number generation for key creation.
    \item Our \texttt{Frequency} module that provides a function for frequency analysis.
\end{itemize}

\begin{code}
module VigenereCipher where

import System.IO
import Data.List (nub, sort, sortBy, groupBy, maximumBy, tails, minimumBy)
import Data.Char (ord, chr, isAlpha, toUpper, toLower)
import Data.Ord (comparing)
import Data.Function (on)
import System.Random (randomRs, newStdGen)
import qualified Data.Map as M 
import Frequency (findBestShift)
\end{code}

\subsubsection{Utility Functions and Constants}
Constants such as \texttt{baseChar} and \texttt{endChar} define the range of uppercase letters. The function \texttt{shiftChar} applies a shift to a character, wrapping around if necessary.

\begin{code}
baseChar, endChar :: Char
baseChar = 'A'
endChar = 'Z'
baseVal, range :: Int
baseVal = ord baseChar
range = 26

shiftChar :: Int -> Char -> Char
shiftChar s c
  | isAlpha c = let upperC = toUpper c
                in chr $ baseVal + (ord upperC - baseVal + s) `mod` range
  | otherwise = c
\end{code}

\subsubsection{Encryption and Decryption}
The functions \texttt{vigenereEncrypt} and \texttt{vigenereDecrypt} perform the core operations:
\begin{itemize}
    \item \textbf{Encryption:} The plaintext is first cleaned (non-alphabetic characters removed and converted to uppercase). The encryption is achieved by cycling through the key and shifting each character by the value corresponding to the key letter.
    \item \textbf{Decryption:} This function reverses the encryption by applying the negative of the shift.
\end{itemize}

\begin{code}
vigenereEncrypt :: String -> String -> String
vigenereEncrypt key plaintext
  | null key  = error "Empty key not allowed"
  | otherwise = encrypt plaintext (cycle key)
  where
    encrypt [] _ = []
    encrypt (_:_) [] = error "Unexpected empty key during encryption"
    encrypt (c:cs) ks@(k:ks')
      | isAlpha c = shiftChar (ord (toUpper k) - baseVal) (toUpper c) : encrypt cs ks'
      | otherwise = c : encrypt cs ks

vigenereDecrypt :: String -> String -> String
vigenereDecrypt key ciphertext
  | null key  = error "Empty key not allowed"
  | otherwise = decrypt ciphertext (cycle key)
  where
    decrypt [] _ = []
    decrypt (_:_) [] = error "Unexpected empty key during decryption"
    decrypt (c:cs) ks@(k:ks')
      | isAlpha c = shiftChar (- (ord (toUpper k) - baseVal)) c : decrypt cs ks'
      | otherwise = c : decrypt cs ks
\end{code}

\subsubsection{Finding Repeated Sequences and Calculating Distances}
These functions implement the Kasiski examination, a classical method for breaking polyalphabetic ciphers such as the Vigenère cipher by exploiting repeated sequences in the ciphertext.

\paragraph{Theory:}
\begin{itemize}
  \item \textbf{Repeated Sequences:}  
  When a specific sequence of letters appears more than once in the ciphertext, it is likely that the same portion of the key was used to encrypt different parts of the plaintext. Consequently, the distance (i.e., the number of characters) between these repeated sequences is often a multiple of the key length.
  \item \textbf{Distance Analysis:}  
  By calculating the distances between repeated sequences and then analyzing the common factors among these distances, one can infer the possible length of the key. Typically, the greatest common divisor (or the most common divisor) of these distances is a strong candidate for the key length.
\end{itemize}

\paragraph{Implementation Details:}
\begin{itemize}
  \item \texttt{findRepeatedSequences}:  
  This function takes an integer \texttt{seqLen} representing the length of the sequence to search for. It:
  \begin{enumerate}
    \item Iterates over the ciphertext to extract all substrings of length \texttt{seqLen} along with their starting positions.
    \item Sorts these pairs so that identical sequences are grouped together.
    \item Groups the sorted list by the sequence content using \texttt{groupBy}. Only groups with more than one occurrence (i.e., repeated sequences) are retained, and their starting indices are recorded.
  \end{enumerate}
  
  \item \texttt{calculateDistances}:  
  Once the repeated sequences and their positions are known, this function computes all pairwise distances between the positions of each repeated sequence. These distances are used to detect common divisors, which in turn suggest likely key lengths.
\end{itemize}

\begin{code}
findRepeatedSequences :: Int -> String -> [(String, [Int])]
findRepeatedSequences seqLen ctext =
  [ (seqText, positions)
  | group <- groupedSequences
  , let seqText = fst (head group)
  , let positions = map snd group
  , length positions > 1
  ]
  where
    allSequences = [(take seqLen $ drop i ctext, i) | i <- [0..length ctext - seqLen]]
    sorted = sortBy (compare `on` fst) allSequences
    groupedSequences = groupBy ((==) `on` fst) sorted

calculateDistances :: [(String, [Int])] -> [Int]
calculateDistances = concatMap (\(_, positions) ->
  [pos2 - pos1 | (pos1:rest) <- tails positions, pos2 <- rest])
\end{code}

\subsubsection{Frequency Analysis and Key Length Estimation}
These functions employ statistical measures to refine the key length estimation and further assist in breaking the cipher.

\paragraph{Theory:}
\begin{itemize}
  \item \textbf{Index of Coincidence (IC):}  
  The IC is a measure of the probability that two randomly selected letters from a text are the same. For a language like English, the IC is typically around 0.067. A lower IC indicates a more uniform distribution of letters, as seen in well-encrypted text, while a higher IC suggests a distribution similar to natural language.
  \item \textbf{Friedman Test:}  
  This test calculates the IC for columns of ciphertext. When the ciphertext is divided based on a guessed key length, each column ideally represents text encrypted with the same Caesar shift. The average IC of these columns is then compared to the expected value for the language. A key length that yields an average IC close to the expected value is more likely to be correct.
  \item \textbf{Combining Methods:}  
  By merging the insights from the Kasiski examination (which provides concrete candidate key lengths from repeated patterns) and the Friedman test (which statistically evaluates each candidate's plausibility), one can robustly guess the key length.
\end{itemize}

\paragraph{Implementation Details:}
\begin{itemize}
  \item \texttt{indexOfCoincidence}:  
  This function calculates the IC for a given text. It works by:
  \begin{enumerate}
    \item Counting the frequency of each character in the text.
    \item Using these counts to compute the probability that two letters picked at random are the same. This involves summing up $n(n-1)$ for each character count $n$, and dividing by the total number of pairs, $N(N-1)$, where $N$ is the length of the text.
  \end{enumerate}
  
  \item \texttt{friedmanTest}:  
  The Friedman test function divides the ciphertext into several columns, each corresponding to a fixed position in the repeating key cycle. It computes the IC for each column and returns the average IC across all columns. This average is used to gauge how well a particular guessed key length fits the statistical profile of natural language text.
  
  \item \texttt{guessKeyLength}:  
  This function synthesizes the candidate key lengths from both the Kasiski examination and a range of possible lengths evaluated via the Friedman test:
  \begin{enumerate}
    \item It first obtains candidates from the Kasiski method by analyzing repeated sequences and calculating their most common divisor.
    \item It then tests a range of key lengths (e.g., 1 through 20) using the Friedman test, computing a score based on the deviation of the average IC from the expected 0.067.
    \item The key length with the smallest deviation (i.e., the score closest to the expected value) is selected as the best guess.
  \end{enumerate}
\end{itemize}

\begin{code}
indexOfCoincidence :: String -> Double
indexOfCoincidence text =
  let counts = M.fromListWith (+) [(c, 1) | c <- text]
      total = fromIntegral (length text)
      combinations n = n * (n - 1)
  in if total < 2 then 0 
     else (sum [combinations cnt | cnt <- M.elems counts]) / (total * (total - 1))

friedmanTest :: String -> Int -> Double
friedmanTest ctext klength =
  let columns = [everyNth klength i ctext | i <- [0..klength-1]]
      columnICs = map indexOfCoincidence columns
  in sum columnICs / fromIntegral klength
  where
    everyNth n k = map head . takeWhile (not.null) . iterate (drop n) . drop k

guessKeyLength :: String -> Int
guessKeyLength ctext =
  let kasiskiCandidates = take 3 $ kasiskiMethod ctext
      friedmanCandidates = [1..20]
      allCandidates = nub (kasiskiCandidates ++ friedmanCandidates)
      scores = [(kl, abs (friedmanTest ctext kl - 0.067)) | kl <- allCandidates]
  in fst $ minimumBy (comparing snd) scores
  where
    kasiskiMethod ct =
      let sequences = findRepeatedSequences 3 ct
          distances = calculateDistances sequences
      in if null distances then [3] else [mostCommonDivisor distances]

mostCommonDivisor :: [Int] -> Int
mostCommonDivisor distances =
  fst $ maximumBy (comparing snd) divisorCounts
  where
    divisors n = [d | d <- [1..n], n `mod` d == 0]
    allDivisors = concatMap divisors distances
    sortedDivisors = sort allDivisors
    divisorCounts = [(d, count) | (d:ds) <- groupBy (==) sortedDivisors, 
                     let count = length (d:ds)]
\end{code}

\subsubsection{Key Guessing and Cracking the Cipher}
To crack the Vigenère cipher:
\begin{itemize}
    \item \texttt{guessShift} determines the most likely Caesar shift for a given column of ciphertext using frequency analysis.
    \item \texttt{crackVigenere}:
    \begin{enumerate}
      \item Normalizes the ciphertext.
      \item Estimates the key length using the methods described above.
      \item Splits the ciphertext into columns corresponding to each key character.
      \item Determines the best shift for each column.
      \item Reconstructs the key from these shifts.
    \end{enumerate}
\end{itemize}

\begin{code}
guessShift :: String -> Int
guessShift column = 
  findBestShift (map toLower column)  -- Frequency module expects lowercase

shiftToKey :: Int -> Char
shiftToKey shift = chr (ord 'A' + shift)

crackVigenere :: String -> String
crackVigenere ciphertext =
  let
      cleanText = map toUpper (filter isAlpha ciphertext)
      keyLen = guessKeyLength cleanText
      columns = [ everyNth keyLen i cleanText | i <- [0..keyLen-1] ]
      shifts = map (findBestShift . map toLower) columns
      key = map shiftToKey shifts
  in key
  where
    everyNth n k xs = map head $ takeWhile (not . null) $ iterate (drop n) (drop k xs)

normalize :: String -> String
normalize = map toLower . filter isAlpha
\end{code}

\subsubsection{I/O Operations}
The module includes functions for file input and output:
\begin{itemize}
    \item \texttt{generateVigenereKeyIO} creates a random key of a specified length.
    \item \texttt{encryptIO} and \texttt{decryptIO} perform encryption and decryption on files using a key stored in another file.
    \item \texttt{crackIO} attempts to crack a ciphertext file by guessing the key and then decrypting the file.
\end{itemize}

\begin{code}
generateVigenereKeyIO :: Int -> IO String
generateVigenereKeyIO keyLength =
  take keyLength . randomRs (baseChar, endChar) <$> newStdGen

crackIO :: String -> String -> IO ()
crackIO output inputFile = do
    ciphertext <- readFile inputFile
    let guessedKey = crackVigenere ciphertext
        guessedKeyLen = length guessedKey
        plaintext = vigenereDecrypt guessedKey ciphertext
    writeFile output plaintext
    putStrLn $ "Ciphertext: " ++ ciphertext
    putStrLn $ "Guessed key: " ++ guessedKey
    putStrLn $ "Guessed key length: " ++ show guessedKeyLen
    putStrLn $ "Guessed plaintext: " ++ plaintext

encryptIO :: String -> String -> String -> IO ()
encryptIO output inputFile keyFile = do
    inputContent <- readFile inputFile
    keyContent <- readFile keyFile
    writeFile output (vigenereEncrypt keyContent inputContent)

decryptIO :: String -> String -> String -> IO ()
decryptIO output inputFile keyFile = do
    inputContent <- readFile inputFile
    keyContent <- readFile keyFile
    writeFile output (vigenereDecrypt keyContent inputContent)
\end{code}

\subsubsection{Main Function}
The \texttt{vig} function, works as entry point, just like the \texttt{caesar} and \texttt{otp} functions in the Caesar and OTP modules. It provides a simple command-line interface to generate keys, encrypt, decrypt, or crack Vigenère ciphers. Due to repetitive code, we have omitted the full implementation here.

\hide{
\begin{code}
vig :: IO ()
vig = do
    hSetBuffering stdin LineBuffering
    putStrLn "[Vigenere] Do you want to generate a key, encrypt, decrypt, or crack? (generate/encrypt/decrypt/crack)"
    method <- getLine
    case method of
        "generate" -> do
            putStrLn "In what file do you want to store the key? (e.g., key.txt)"
            keyFile <- getLine
            putStrLn "Enter desired key length (recommended 8-16 characters):"
            keyLength <- read <$> getLine
            key <- generateVigenereKeyIO keyLength
            writeFile keyFile key

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

        "crack" -> do
            putStrLn "Where should the decrypted text be saved? (e.g., output.txt)"
            outputFile <- getLine
            putStrLn "What ciphertext file should be cracked? (e.g., cipher.txt)"
            inputFile <- getLine
            crackIO outputFile inputFile

        _ -> putStrLn "Invalid method. Please choose 'generate', 'encrypt', 'decrypt', or 'crack'."
\end{code}
}
