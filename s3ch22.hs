import Crypto
import Control.Concurrent (threadDelay)
import System.Random
import Data.Time.Clock.POSIX
import qualified Data.Word as W

crack :: W.Word32 -> Integer -> W.Word32
crack n t = helper (fromIntegral t)
  where helper x = if (fst . extractMT $ initMT x) == n
                     then x
                     else helper (x-1)

main :: IO ()
main = do
    delay  <- randomRIO (40, 1000) :: IO Int
    delay' <- randomRIO (40, 1000) :: IO Int
    threadDelay (10^6*delay)
    seed  <- fmap round getPOSIXTime
    let mtState = initMT seed
    threadDelay (10^6*delay')
    let n = fst . extractMT $ mtState
    print n
    now <- fmap round getPOSIXTime
    print $ crack n now

