import Crypto
import qualified Data.Sequence as S
import qualified Data.Word as W

getNums :: (Int, S.Seq W.Word32) -> [W.Word32]
getNums st = helper 0 st
  where helper i state
          | i == 624  = []
          | otherwise = n : helper (i+1) state'
          where (n, state') = extractMT state

crack :: [W.Word32] -> [W.Word32]
crack = map untemper

main :: IO ()
main = do
    let state = initMT 3454584
        nums  = getNums state
    print $ (S.fromList $ crack nums) == (genNumsMT . snd $ state)

