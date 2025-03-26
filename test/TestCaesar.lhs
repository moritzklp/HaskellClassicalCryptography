\begin{code}
module TestCaesar (spec) where

import Test.Hspec
import Test.QuickCheck
import CaesarCipher
import Data.Char (toUpper)

spec :: Spec
spec = do
  it "Encrypts 'HELLO' with shift 3 to 'KHOOR'" $
    caesarEncrypt 3 "HELLO" `shouldBe` "KHOOR"

  it "Decrypts 'KHOOR' with shift 3 to 'HELLO'" $
    caesarDecrypt 3 "KHOOR" `shouldBe` "HELLO"

  it "Encrypt-then-decrypt gives back original" $
    property $ \n msg ->
      let shift = (abs n `mod` 25) + 1
          cleaned = map toUpper $ filter (`elem` ['A'..'Z']) (msg :: String)
      in caesarDecrypt shift (caesarEncrypt shift cleaned) == cleaned
\end{code}
