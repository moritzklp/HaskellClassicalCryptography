\begin{code}
module TestMTP (spec) where

import Test.Hspec
import MTP (breakWithPartialKey)
import Data.Char (ord)
import Data.Bits (xor)

spec :: Spec
spec = do
  it "Partial decryption with correct key reveals plaintext letters" $
    let plain = "HELLO"
        key = map (fromIntegral . ord) "XMCKL"
        cipher = zipWith (\p k -> fromIntegral (ord p) `xor` k) plain key
        partialKey = map Just key
        result = breakWithPartialKey cipher partialKey
    in result `shouldBe` plain
\end{code}