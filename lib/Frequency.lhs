
\section{Caesar Cipher Frequency Analysis}
The idea here is to exploit the fact that in English (and most other natural languages), certain letters appear with predictable frequencies. For example, the letter 'e' is the most common letter in English text, followed by 't', 'a', 'o', etc.
In the code we define an expected frequency distribution for English letters (from Wikipedia). 

\begin{code}
module Frequency where

import Data.Char
import Data.List
import Data.Ord

-- | Frequency table of English letters (source: https://en.wikipedia.org/wiki/Letter_frequency)
englishFrequencies :: [(Char, Double)]
englishFrequencies = [('a', 8.2), ('b', 1.5), ('c', 2.8), ('d', 4.3), ('e', 12.7), ('f', 2.2), ('g', 2.0), ('h', 6.1),
 ('i', 7.0), ('j', 0.15), ('k', 0.77), ('l', 4.0), ('m', 2.4), ('n', 6.7), ('o', 7.5), ('p', 1.9),
 ('q', 0.095), ('r', 6.0), ('s', 6.3), ('t', 9.1), ('u', 2.8), ('v', 0.98), ('w', 2.4), ('x', 0.15),
 ('y', 2.0), ('z', 0.074)]
\end{code}

Next, we filter the text to keep only the alphabetic characters and convert them to lowercase. This ensures that the frequency analysis is case-insensitive and not affected by non-alphabetic characters.
The \texttt{normalize} function removes any non-alphabetical characters and converts the remaining ones to lowercase. This is very important for analysing how often words appear in a text.
The \texttt{charFrequency} function then calculates how often a specific character appears in the text (after normalization) in relation to the total number of letters.
The \texttt{letterFrequencies} function builds a list of tuples for every letter from 'a' to 'z', each paired with its computed frequency in the text.

\begin{code}
-- | Keep the lowercase (alphabetic) characters 
normalize :: String -> String
normalize = map toLower . filter isAlpha

-- | Count the occurrences of a given character in a string
countOccurrences :: Char -> String -> Int
countOccurrences ch = length . filter (== ch)

-- | Calculate the frequency of a given character in the text
-- | 'fromIntegral' takes integral value (like 'Int' or 'Integer') and converts into more general numeric type ('Double' in our case)
charFrequency :: Char -> String -> Double
charFrequency c text =
  let norm = normalize text
      total = fromIntegral (length norm)
      count = fromIntegral (countOccurrences c norm)
  in count / total

-- | Get the frequency distribution for all the letters a-z
letterFrequencies :: String -> [(Char, Double)]
letterFrequencies text = [(c, charFrequency c text) | c <- ['a'..'z']]
\end{code}

In the next part, we compare the observed frequencies (from a decrypted candidate) with the expected letter frequencies in standard English. This comparison helps us evaluate how ``English-like'' the decrypted text is. 
The method we use is based on the \textit{chi-squared statistic}, a technique often used in statistical hypothesis testing. The lower the chi-squared value, the closer the candidate is to what is typical in English.\footnote{Idea adapted from \url{http://practicalcryptography.com/cryptanalysis/text-characterisation/chi-squared-statistic/}.}

\begin{code}
-- | Calculate the chi-squared statistic between the observed and expected frequencies
-- | 'zip' pairs each observed frequency with its corresponding expected frequency - lets us compare the same letter's frequency from both lists
chiSquared :: [(Char, Double)] -> [(Char, Double)] -> Double
chiSquared observed expected =
  sum [((o - e) ** 2) / e | ((_, o), (_, e)) <- zip observed expected]
\end{code}

In \texttt{shiftChar} we handle the rotation of a single alphabetic character by a specified amount, wrapping around the alphabet if necessary. This function first checks whether the character is alphabetic; if it is, it calculates its position (0–25) by subtracting the base ASCII value for uppercase or lowercase letters. It then subtracts the shift amount, applies a modulo 26 to ensure the value wraps around properly, and adds the base back to convert it back to a valid ASCII character. Non-alphabetical characters are returned unchanged. 
The function \texttt{decryptWithShift} then applies \texttt{shiftChar} across the entire ciphertext, trying one candidate decryption by applying a given shift.

\begin{code}
-- | Shift a character by a given amount
shiftChar :: Int -> Char -> Char
shiftChar shift c
  | not (isAlpha c) = c 
  | otherwise = chr (base + mod (ord c - base - shift) 26)
  where
    base = if isUpper c then ord 'A' else ord 'a'


-- | Decrypt the text by applying a shift to each character
decryptWithShift :: Int -> String -> String
decryptWithShift shift = map (shiftChar shift)
\end{code}

Now \texttt{findBestShift} tests all 26 possible shifts. For each shift, it decrypts the ciphertext, computes the frequency distribution, and calculates the chi-squared statistic based on the expected frequencies. The shift with the lowest score is assumed to be the correct one.
This function is then used in \texttt{getCaesarDecryptedText} to decrypt the ciphertext using the best shift found.

\begin{code}
-- | Find the best shift by comparing the frequency distributions (using chi-squared)
-- | 'minimumBy' finds the minimum element in a list based on a comparison function (in this case, the chi-squared value), return the shift with the lowest score
findBestShift :: String -> Int
findBestShift ciphertext = let scores = [ (shift, chiSquared (letterFrequencies (decryptWithShift shift ciphertext)) englishFrequencies) | shift <- [0..25] ]
  in fst (minimumBy (comparing snd) scores)

-- | Decrypt the Caesar cipher text using the frequency analysis (find the best shift and decrypt)
getCaesarDecryptedText :: String -> String
getCaesarDecryptedText ciphertext =
  let bestShift = findBestShift ciphertext
  in decryptWithShift bestShift ciphertext

-- | Just for testing
freq :: IO ()
freq = do
    putStrLn "Enter the encrypted message:"
    encrypted <- getLine
    putStrLn "Decrypted message:"
    putStrLn $ getCaesarDecryptedText encrypted
\end{code}
