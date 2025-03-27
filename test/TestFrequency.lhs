\begin{code}
module TestFrequency (spec) where

import Test.Hspec
import Frequency
import CaesarCipher (caesarEncrypt)
import Data.Char (toLower, toUpper)

spec :: Spec
spec = do
  it "findBestShift detects Caesar shift" $
    let plain = "ATTACKATDAWN"
        shift = 7
        enc = caesarEncrypt shift plain
    in findBestShift (map toLower enc) `shouldBe` shift

  it "getCaesarDecryptedText recovers original message" $
    let plain = "DEFENDTHEBASE"
        enc = caesarEncrypt 4 plain
        dec = getCaesarDecryptedText (map toLower enc)
    in map toUpper dec `shouldBe` map toUpper plain

  it "Detects correct shift even in long, natural language" $
      let text = replicate 10 "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG "
          plain = concat text
          shift = 13
          enc = caesarEncrypt shift plain
          guessed = findBestShift (map toLower enc)
      in guessed `shouldBe` shift
    
  it "findBestShift returns 0 when given already-decrypted text" $
      findBestShift "this is already english" `shouldBe` 0

  it "getCaesarDecryptedText works on mixed-case input" $
      let msg = "SeCrEtMeSsAgE"
          enc = caesarEncrypt 5 msg
          cracked = getCaesarDecryptedText (map toLower enc)
      in map toUpper cracked `shouldBe` map toUpper msg
\end{code}