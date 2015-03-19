import Crypto
import System.IO
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = do
  withFile "7.txt" ReadMode (\handle -> do
    c <- hGetContents handle
    contents <- return . fromBase64 . concat . lines $ c
    let key = aesKey . C8.pack $ "YELLOW SUBMARINE"
        pt = C8.unpack $ aesECBDecrypt key contents
    putStrLn pt
    )
