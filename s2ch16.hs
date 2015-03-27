import Crypto
import System.Entropy
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as B

{-crack :: B.ByteString -> [B.ByteString]-}
crack k ct = crackCBCPaddingOracle k ct

main :: IO ()
main = do
  (k, ct) <- randCBCEncrypt
  print $ crack k ct
