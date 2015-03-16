import Crypto
import qualified Data.List as L
import System.IO
import Data.Ord

main :: IO ()
main = do
  withFile "4.txt" ReadMode (\handle -> do
    contents <- hGetContents handle
    let encs = L.lines contents
    let decoded = L.concatMap (\x -> let decList = decodeSingleByteXor x in map (\y -> (x, y)) decList) encs
    let top = take 5 . L.sortBy (comparing (fst . snd)) $ decoded
    mapM_ print top
    )
