\begin{code}
module TestVigenere (spec) where

import Test.Hspec
import VigenereCipher

spec :: Spec
spec = do
  it "Encrypts and decrypts with Vigenère cipher" $
    let key = "LEMON"
        msg = "ATTACKATDAWN"
        enc = vigenereEncrypt key msg
        dec = vigenereDecrypt key enc
    in dec `shouldBe` msg
\end{code}