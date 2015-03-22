import Crypto
import System.Entropy
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as B


main :: IO ()
main = do
  k <- getEntropy 16
  let key = aesKey k
      blockSize = getBlockSizeSimple aesByteAtATimeECBEncryptSimple key
      ecbMode = isAESInECB . aesByteAtATimeECBEncryptSimple key $ C8.pack "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  putStrLn . C8.unpack . aesByteAtATimeECBDecryptSimple blockSize $ key
