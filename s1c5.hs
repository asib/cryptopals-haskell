import Crypto
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = do
  s <- return $ C8.pack "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
  k <- return $ LC8.pack "ICE"
  c <- return $ repeatingKeyXor k s
  print $ toHex c
