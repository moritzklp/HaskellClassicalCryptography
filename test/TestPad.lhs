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
\end{code}