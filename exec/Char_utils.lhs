Module for char_utils analysis

\begin{code}
module Char_utils where

import Data.Char (chr)
import Data.List (elemIndex, sortBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Char (toLower)

-- all printable ascii characters for frequency analysis
printableASCII :: [Char]
printableASCII = [chr 32 .. chr 126]

capitalLetters :: [Char]
capitalLetters = [chr 65 .. chr 90]

letters :: [Char]
letters = [chr 97 .. chr 122]

lettersByFrequency :: [Char]
lettersByFrequency = "etaoinshrdlucmfwypvbgkqjxz"

switchCase :: Char -> Char
switchCase c | c `elem` capitalLetters = letters!!(fromJust (elemIndex c capitalLetters))
           | c `elem` letters = capitalLetters!!(fromJust (elemIndex c letters))
           | otherwise = ' '

getCharFrequency1 :: Char -> String -> (Char, Int)
getCharFrequency1 c x = (c, length (filter (\n -> n == c || n == switchCase c) x))

mergeResults :: [[(Char, Int)]] -> [(Char, Int)]
mergeResults xs = map (\n -> (fst (head n), sum(map snd n))) xs

getCharFrequency :: [String] -> [(Char, Int)] 
getCharFrequency xs = mergeResults $ map (\n -> map (getCharFrequency1 n) xs) letters 

-- helper function found online
oneList :: [a] -> [b] -> [(a, b)]
oneList []     _      = []
oneList (x:xs) (y:ys) = (x, y) : oneList xs ys
oneList (_:_) _ = []

frequencyAnalysis :: [(Char, Int)] -> [(Char, Char)]
frequencyAnalysis xs = 
    let sorted = sortBy (flip (comparing snd)) xs
        sortedLetters = map fst sorted
    in oneList sortedLetters lettersByFrequency

substitute :: [Char] -> [(Char, Char)] -> [Char]
substitute xs dict = map (\x -> case lookup x dict of Just y  -> y
                                                      Nothing -> x) xs

fullFrequencyAttack :: [[Char]] -> [[Char]]
fullFrequencyAttack xs =     
    let freq = getCharFrequency xs
        dict = frequencyAnalysis freq
    in map ((`substitute` dict) . (map toLower) ) xs

\end{code}

