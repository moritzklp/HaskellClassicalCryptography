\begin{code}
module Vigenere_cipher where

import System.IO
import Data.List (nub, sort, sortBy, groupBy, maximumBy, transpose, tails, minimumBy)
import Data.Char (ord, chr, isAlpha, toUpper)
import Data.Ord (comparing)
import Data.Function (on)
import System.Random (randomRs, newStdGen)
import qualified Data.Map as M 

baseChar, endChar :: Char
baseChar = '!'
endChar = '~'
baseVal, range :: Int
baseVal = ord baseChar
range = ord endChar - ord baseChar + 1

shiftChar :: Int -> Char -> Char
shiftChar s c = chr $ baseVal + (ord c - baseVal + s) `mod` range

vigenereEncrypt :: String -> String -> String
vigenereEncrypt key plaintext =
  zipWith (\k c -> shiftChar (ord k - baseVal) c) (cycle key) plaintext

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
guessShift group =
  let score shift = 
        let decryptedGroup = map (shiftChar (-shift)) group
        in sum [1 | c <- decryptedGroup, isAlpha c]
      scoredShifts = [(shift, score shift) | shift <- [0..range-1]]
  in fst $ maximumBy (comparing snd) scoredShifts

crackVigenere :: String -> String
crackVigenere ctext =
  let keyLen = guessKeyLength ctext
      columns = transpose [everyNth keyLen i ctext | i <- [0..keyLen-1]]
      shifts = map guessShift columns
  in map (\s -> chr (baseVal + s)) shifts
  where
    everyNth n k = map head . takeWhile (not.null) . iterate (drop n) . drop k

-- IO Operations --------------------------------------------------------------
generateVigenereKeyIO :: Int -> IO String
generateVigenereKeyIO keyLength =
  take keyLength . randomRs (baseChar, endChar) <$> newStdGen

crackIO :: String -> String -> IO ()
crackIO output inputFile = do
    ciphertext <- readFile inputFile
    let guessedKey = crackVigenere ciphertext
        plaintext = vigenereDecrypt guessedKey ciphertext
    writeFile output plaintext
    putStrLn $ "Guessed key: " ++ guessedKey
    
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
    putStrLn "Hello, do you want to generate a key, encrypt, or decrypt? (generate/encrypt/decrypt)"
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
        _ -> putStrLn "Invalid method. Please choose 'generate', 'encrypt', or 'decrypt'."


vig2 = do
  ctext <- readFile "output_vig.txt"
  putStrLn $ "Estimated key length: " ++ show (guessKeyLength ctext)
\end{code}