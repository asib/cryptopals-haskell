import Crypto
import System.IO
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as B

getECBEncryptedCipher :: [B.ByteString] -> B.ByteString
getECBEncryptedCipher (c:cs)
  | r = c
  | otherwise = getECBEncryptedCipher cs
    where r = isAESInECB c


main :: IO ()
main = do
  withFile "10.txt" ReadMode (\handle -> do
    c <- hGetContents handle
    contents <- return . fromBase64 . concat . lines $ c
    let key = aesKey . C8.pack $ "YELLOW SUBMARINE"
    putStrLn . C8.unpack . aesCBCDecrypt key 0 $ contents
    )
