Module for char_utils analysis

\begin{code}
module Char_utils where

import Data.Char (chr, ord)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

-- all printable ascii characters for frequency analysis
printableASCII :: [Char]
printableASCII = [chr 32 .. chr 126]

capitalLetters :: [Char]
capitalLetters = [chr 65 .. chr 90]

letters :: [Char]
letters = [chr 97 .. chr 122]

switchCase :: Char -> Char
switchCase c | c `elem` capitalLetters = letters!!(fromJust (elemIndex c capitalLetters))
           | c `elem` letters = capitalLetters!!(fromJust (elemIndex c letters))
           | otherwise = ' '

getCharFrequency1 :: Char -> String -> (Char, Int)
getCharFrequency1 c x = (c, length (filter (\n -> n == c || n == switchCase c) x))

getCharFrequency :: [String] -> [(Char, Int)] 
getCharFrequency x = undefined -- map (length filter ) letters


\end{code}

