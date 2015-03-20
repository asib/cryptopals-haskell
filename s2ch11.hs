import Crypto
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as B


main :: IO ()
main = do
  ct <- aesEncryptRandom $ C8.pack "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  print $ isAESInECB ct
