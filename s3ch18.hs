import Crypto
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as B
import qualified Data.LargeWord as LGW
import qualified Data.Word as W

crack :: LGW.Word128 -> W.Word64 -> B.ByteString -> B.ByteString
crack key nonce ct = aesCTR key nonce ct

main :: IO ()
main = do
  print $ crack (aesKey . C8.pack $ "YELLOW SUBMARINE") 0 (fromBase64 "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ==")
