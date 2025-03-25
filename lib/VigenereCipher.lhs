\begin{code}
module VigenereCipher where

import System.IO
import Data.List (nub, sort, sortBy, groupBy, maximumBy, transpose, tails, minimumBy)
import Data.Char (ord, chr, isAlpha, toUpper, toLower)
import Data.Ord (comparing)
import Data.Function (on)
import System.Random (randomRs, newStdGen)
import qualified Data.Map as M 
import Frequency (findBestShift, normalize, letterFrequencies, chiSquared, englishFrequencies)


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

vigenereEncrypt :: String -> String -> String
vigenereEncrypt key plaintext =
  let cleaned = map toUpper $ filter isAlpha plaintext
  in zipWith (\k c -> shiftChar (ord k - baseVal) c) (cycle key) cleaned

vigenereDecrypt :: String -> String -> String
vigenereDecrypt key ciphertext =
  zipWith (\k c -> shiftChar (- (ord k - baseVal)) c) (cycle key) ciphertext

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

mostCommonDivisor :: [Int] -> Int
mostCommonDivisor distances =
  fst $ maximumBy (comparing snd) divisorCounts
  where
    divisors n = [d | d <- [1..n], n `mod` d == 0]
    allDivisors = concatMap divisors distances
    sortedDivisors = sort allDivisors
    divisorCounts = [(d, count) | (d:ds) <- groupBy (==) sortedDivisors, 
                     let count = length (d:ds)]

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
      scores = [(kl, abs (friedmanTest ctext kl - 0.0667)) | kl <- allCandidates]
  in fst $ minimumBy (comparing snd) scores
  where
    kasiskiMethod ctext = 
      let sequences = findRepeatedSequences 3 ctext
          distances = calculateDistances sequences
      in if null distances then [3] else [mostCommonDivisor distances]

guessShift :: String -> Int
guessShift column = 
  findBestShift (map toLower column)  -- Frequency module expects lowercase

crackVigenere :: String -> String
crackVigenere ciphertext =
  let
      cleanText = map toUpper (filter isAlpha ciphertext)
      -- Guess key length
      keyLen = guessKeyLength cleanText
      -- Split into columns
      columns = [ everyNth keyLen i cleanText | i <- [0..keyLen-1] ]
      -- Find best shift for each column
      shifts = map (findBestShift . map toLower) columns  -- Frequency expects lowercase
      -- Convert shifts to key
      key = map shiftToKey shifts
  in key
  where
    everyNth n k xs = map head $ takeWhile (not . null) $ iterate (drop n) (drop k xs)
    shiftToKey shift = chr (ord 'A' + shift)

shiftToKey :: Int -> Char
shiftToKey shift = chr (ord 'A' + shift)

normalize :: String -> String
normalize = map toLower . filter isAlpha


-- IO Operations --------------------------------------------------------------
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
    putStrLn $ "Guessed key: " ++ guessedKey
    putStrLn $ "Guessed key length: " ++ show guessedKeyLen

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

-- Main Function with Proper Key Generation ------------------------------------
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