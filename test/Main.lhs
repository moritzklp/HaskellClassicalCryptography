\section{Tests}

This section contains tests for the different modules we implemented.

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
  describe "Vigenere Cipher Tests" TestVigenere.spec
  describe "OTP Tests" TestOTP.spec
  describe "MTP Tests" TestMTP.spec
  describe "Pad (XOR Helper) Tests" TestPad.spec
\end{code}

For the Caesar and Vigenère ciphers, we test that the encryption and decryption functions work correctly, even with mixed case and punctuation. 
We also check the frequency analysis to see if it can crack Caesar ciphers automatically. 
The OTP tests ensure that the One-Time Pad algorithm behaves as expected, including partial decryption of real ciphertexts. 
For the Many-Time Pad attack, we test a given set of challenge ciphertexts and check if our code can recover at least 75\% of the original message. 
One of the tests compares the output to a known Albert Einstein quote to see if it matches well.
Finally, we test the XOR helper functions to ensure they're reversible and correct.
These tests help us ensure that our code works as intended.
