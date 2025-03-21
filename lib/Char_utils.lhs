Module for char_utils analysis

\begin{code}
module Char_utils where

import Data.Char (chr)
import Data.List (elemIndex, sortBy, find)
import Data.Maybe (fromJust, mapMaybe)
import Data.Ord (comparing)
import Data.Char (toLower)


-- | All uppercase letters in the English alphabet for frequency analysis
capitalLetters :: [Char]
capitalLetters = [chr 65 .. chr 90]

-- | All lowercase letters in the English alphabet for frequency analysis
letters :: [Char]
letters = [chr 97 .. chr 122]

-- | All letters in the English in the order of the frequency of their occurance
lettersByFrequency :: [Char]
lettersByFrequency = "etaoinshrdlucmfwypvbgkqjxz"

-- | Helper function to change the case of a letter
switchCase :: Char -> Char
switchCase c | c `elem` capitalLetters = letters!!(fromJust (elemIndex c capitalLetters))
           | c `elem` letters = capitalLetters!!(fromJust (elemIndex c letters))
           | otherwise = ' '

-- | Checks the frequency of the given character in the given string
getCharFrequencySingle :: Char -> String -> (Char, Int)
getCharFrequencySingle c x = (c, length (filter (\n -> n == c || n == switchCase c) x))

-- | Flatten a lists of frequencies for multiple strings to a single list
mergeResults :: [[(Char, Int)]] -> [(Char, Int)]
mergeResults xs = map (\n -> (fst (head n), sum(map snd n))) xs

-- | Counts the occurance of all letters in the given strings
getCharFrequency :: [String] -> [(Char, Int)] 
getCharFrequency xs = mergeResults $ map (\n -> map (getCharFrequencySingle n) xs) letters

sortLettersByOccurance :: [(Char, Int)] -> [Char]
sortLettersByOccurance xs = map fst (sortBy (flip (comparing snd)) xs)

-- | Takes the list of characters and their frequency and matches a second letter based on the frequency in English
frequencyAnalysis :: [(Char, Int)] -> [(Char, Char)]
frequencyAnalysis xs = zip (sortLettersByOccurance xs) lettersByFrequency

-- | Takes in a string and a dictionary of characters and converts each character in the string
substitute :: [Char] -> [(Char, Char)] -> [Char]
substitute xs dict = map (\x -> case lookup x dict of Just y  -> y
                                                      Nothing -> x) xs
-- | Performs frequency analysis on the provided strings and returns the strings with substituted characters
fullFrequencyAttack :: [[Char]] -> [[Char]]
fullFrequencyAttack xs =     
    let freq = getCharFrequency xs
        dict = frequencyAnalysis freq
    in map ((`substitute` dict) . (map toLower) ) xs

compareOccurances :: [Char] -> [Char] -> Int
compareOccurances (x:xs) (y:ys) = (if x==y then (length xs) else 0) + compareOccurances xs ys
compareOccurances x y = if x==y then 1 else 0
-- compareOccurances _ _ = 0

-- | Take a list of strings and sort them by the probability of belonging to English
sortByFrequency :: [String] -> [String]
sortByFrequency xs = 
    let freq = map (\n -> getCharFrequency [n]) xs -- separate letter frequency data for each string
        strWithFreq = zip xs freq
        sortedLetters = map (\n -> (fst n, sortLettersByOccurance (snd n))) strWithFreq -- letters are put to a string sorted by frequency
        weights = map (\n -> (n, compareOccurances lettersByFrequency n)) (map snd sortedLetters)
        sorted = map fst (sortBy (flip (comparing snd)) weights) -- sort by weight, return strings
    in mapMaybe (\x -> fmap fst (find (\(_, v) -> v == x) sortedLetters)) sorted


\end{code}

