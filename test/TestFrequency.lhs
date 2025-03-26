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
\end{code}