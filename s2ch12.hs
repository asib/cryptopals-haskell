import Crypto
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as B


main :: IO ()
main = do
  key <- getEntropy 16
  print $ getBlockSizeSimple aesByteAtATimeEncryptSimple key
