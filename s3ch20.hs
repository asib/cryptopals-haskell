import Crypto
import System.IO
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as B
import qualified Data.List as L

main :: IO ()
main = do
  withFile "20.txt" ReadMode (\handle -> do
    c <- hGetContents handle
    contents <- return . map fromBase64 . lines $ c
    print . take 1 . decodeRepeatingKeyXor . C8.unpack $ foldl B.append B.empty contents
    )

