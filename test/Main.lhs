\begin{code}
module Main where

import Test.Hspec

import qualified TestCaesar
import qualified TestFrequency
import qualified TestVigenere
import qualified TestOTP
import qualified TestMTP
import qualified TestPad
\end{code}

The following uses the HSpec library to define different tests.

\begin{code}
main :: IO ()
main = hspec $ do
  describe "Caesar Cipher Tests" TestCaesar.spec
  describe "Frequency Analysis Tests" TestFrequency.spec
  describe "Vigenère Cipher Tests" TestVigenere.spec
  describe "OTP Tests" TestOTP.spec
  describe "MTP Tests" TestMTP.spec
  describe "Pad (XOR Helper) Tests" TestPad.spec
\end{code}

To run the tests, use \verb|stack test|.