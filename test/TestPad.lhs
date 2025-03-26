\begin{code}
module TestPad (spec) where

import Test.Hspec
import Pad (padString)

spec :: Spec
spec = do
  it "XOR of a string with itself gives zero bytes" $
    padString "AAAA" "AAAA" `shouldBe` "\0\0\0\0"

  it "padString is reversible" $
    let s1 = "message"
        s2 = "mysecret"
        enc = padString s1 s2
        dec = padString enc s2
    in dec `shouldBe` s1

  it "padString handles full printable ASCII correctly" $
      let input = ['!'..'~']  
          key   = reverse input
          out   = padString input key
      in padString out key `shouldBe` input

  it "padString with different chars gives non-zero result" $
      padString "AAAA" "BBBB" `shouldNotBe` "\0\0\0\0"

  it "padString works with symbols and numbers" $
      let msg = "1234!@#$"
          key = "abcdABCD"
          dec = padString (padString msg key) key
      in dec `shouldBe` msg

  it "padString with empty strings gives empty output" $
      padString "" "" `shouldBe` ""

  it "padString truncates or pads key to match input length (manual check)" $
      let input = "abcdefghij"
          key   = "shortkey!!"
          out   = padString input key
      in length out `shouldBe` 10
\end{code}