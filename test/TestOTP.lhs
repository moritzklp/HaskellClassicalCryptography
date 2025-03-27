\begin{code}
module TestOTP (spec) where

import Test.Hspec
import Pad (padString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Bits (xor)

spec :: Spec
spec = do
  it "padString encrypts HELLO with key XMCKL to known ciphertext" $
    let msg = "HELLO"
        key = "XMCKL"
        expected = B.pack [72 `xor` 88, 69 `xor` 77, 76 `xor` 67, 76 `xor` 75, 79 `xor` 76]
        actual = C.pack (padString msg key)
    in actual `shouldBe` expected

  it "padString XOR is symmetric" $
    let msg = "HELLOOTP"
        key = "SOMEKEY!"
        enc = padString msg key
        dec = padString enc key
    in dec `shouldBe` msg

  it "padString with same input gives empty string" $
    padString "ABCDEF" "ABCDEF" `shouldBe` replicate 6 '\0'

  it "Encryption and decryption works with full ASCII symbols" $
      let msg = "!@#ABCdef123"
          key = "RANDOMKEY888"
          enc = padString msg key
          dec = padString enc key
      in dec `shouldBe` msg
\end{code}