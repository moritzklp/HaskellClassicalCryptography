\begin{code}
module TestVigenere (spec) where

import Test.Hspec
import VigenereCipher
import Data.Char (toUpper, isAlpha)

spec :: Spec
spec = do
  it "Correctly decrypts known Vigenère ciphertext with key LEMON" $
    vigenereDecrypt "LEMON" "LXFOPVEFRNHR" `shouldBe` "ATTACKATDAWN"

  it "Correctly encrypts known Vigenère example with key LEMON" $
    vigenereEncrypt "LEMON" "ATTACKATDAWN" `shouldBe` "LXFOPVEFRNHR"

  it "Encrypts and decrypts with Vigenère cipher" $
    let key = "LEMON"
        msg = "ATTACKATDAWN"
        enc = vigenereEncrypt key msg
        dec = vigenereDecrypt key enc
    in dec `shouldBe` msg

  it "Encrypts and decrypts non-uniform case input correctly" $
      let key = "KEY"
          msg = "HeLLoWorLD"
          enc = vigenereEncrypt key msg
          dec = vigenereDecrypt key enc
      in dec `shouldBe` map toUpper (filter isAlpha msg)
\end{code}