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

  it "Handles punctuation and spaces correctly" $
    caesarEncrypt 4 "Hello, World!" `shouldBe` "LIPPS, ASVPH!"
 
  it "Handles wraparound correctly (Z becomes C with shift 3)" $
      caesarEncrypt 3 "XYZ" `shouldBe` "ABC"

  it "Encryption with shift 0 returns same string (identity)" $
      caesarEncrypt 0 "TEST" `shouldBe` "TEST"

  it "Encrypt-then-decrypt gives back original" $
    property $ \n msg ->
      let shift = (abs n `mod` 25) + 1
          cleaned = map toUpper $ filter (`elem` ['A'..'Z']) (msg :: String)
      in caesarDecrypt shift (caesarEncrypt shift cleaned) == cleaned

  it "Inverse of encryption is decryption (full property)" $
      property $ \(NonNegative n) str ->
        let shift = (n `mod` 25) + 1
            cleaned = map toUpper $ filter (`elem` ['A'..'Z']) str
            enc = caesarEncrypt shift cleaned
        in caesarDecrypt shift enc == cleaned

  describe "Key Generation" $ do
    it "generateCaesarKeyIO returns a shift between 1 and 25" $ do
      shiftStr <- generateCaesarKeyIO
      let shift = read shiftStr :: Int
      shift `shouldSatisfy` (\s -> s >= 1 && s <= 25)
\end{code}
