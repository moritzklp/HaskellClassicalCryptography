\begin{code}
module TestOTP (spec) where

import Test.Hspec
import Pad (padString)

spec :: Spec
spec = do
  it "padString XOR is symmetric" $
    let msg = "HELLOOTP"
        key = "SOMEKEY!"
        enc = padString msg key
        dec = padString enc key
    in dec `shouldBe` msg

  it "padString with same input gives empty string" $
    padString "ABCDEF" "ABCDEF" `shouldBe` replicate 6 '\0'
\end{code}