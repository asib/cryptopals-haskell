import Crypto
import System.Entropy
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as B


main :: IO ()
main = do
  k <- getEntropy 16
  let key = aesKey k
      input = (B.replicate 10 120) `B.append` (C8.pack "admin") `B.append` (B.replicate 11 0) `B.append` (C8.pack "xxx")
      (a:b:c:d:_) = bsSplit 16 . getProfile key . C8.unpack $ input
      result = C8.unpack . remakeProfile key $ foldl (\acc x -> acc `B.append` x) B.empty [a,d,c,b]
  putStrLn result
