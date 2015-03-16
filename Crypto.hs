module Crypto
(
toHex
,fromHex
,toBase64
,fromBase64
,fixedXor
,fixedXorByte
,decodeSingleByteXor
,repeatingKeyXor
,hammingDistance
,decodeRepeatingKeyXor
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import qualified Data.Word8 as W8
import qualified Data.Word as W
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Map as M
import qualified Numeric as N
import qualified Data.Bits as BTS
import qualified Text.Printf as TPF
import Data.Ord

charFreqs :: M.Map W.Word8 Float
charFreqs = M.fromList . map (\(x, y) -> (fromIntegral . C.ord $ x, y)) $ [('E', 0.1202), ('T', 0.091), ('A', 0.0812), ('O', 0.0768), ('I', 0.0731), ('N', 0.0695), ('S', 0.06280000000000001), ('R', 0.0602), ('H', 0.0592), ('D', 0.0432), ('L', 0.0398), ('U', 0.0288), ('C', 0.0271), ('M', 0.026099999999999998), ('F', 0.023), ('Y', 0.021099999999999997), ('W', 0.0209), ('G', 0.0203), ('P', 0.0182), ('B', 0.0149), ('V', 0.0111), ('K', 0.0069), ('X', 0.0017000000000000001), ('Q', 0.0011), ('J', 0.001), ('Z', 0.0007000000000000001)]

asciiToUpper x = if x `elem` [97..122] then x-32 else x

isAlpha' x = if x `elem` (concat [[97..122],[65..90]]) then True else False

isPunc x = if x `elem` (concat [[91..96],[123..126],[32..64],[10,13]]) then True else False

toHex :: B.ByteString -> String
toHex = C8.unpack . B16.encode

-- ignore the invalid part
fromHex :: String -> B.ByteString
fromHex = fst . B16.decode . C8.pack

toBase64 :: B.ByteString -> String
toBase64 = C8.unpack . B64.encode

fromBase64 :: String -> B.ByteString
fromBase64 = B64.decodeLenient . C8.pack

fixedXor :: B.ByteString -> B.ByteString -> B.ByteString
fixedXor xs ys = B.pack . map (uncurry BTS.xor) $ B.zip xs ys

fixedXorByte :: W.Word8 -> W.Word8 -> W.Word8
fixedXorByte x y = BTS.xor x y

score :: B.ByteString -> Float
score s = chi2
  where incOrMake a t = if a `M.member` t then M.adjust (+1) a t else M.insert a 1 t
        freqs = B.foldl (\acc x -> incOrMake (asciiToUpper x) acc) M.empty s
        total = M.foldl (+) 0 freqs
        expect = M.map (\a -> fromIntegral . round $ a * total) charFreqs
        chi2 = M.foldlWithKey (\acc k a -> acc + (if tmp k a /= 0 then (tmp k a)/(norm k $ expected k) else 0)) 0 freqs
          where expected k = case M.lookup k expect of
                               Just b -> b
                               Nothing -> 0
                tmp k a = (a - expected k)^2
                norm k a
                  | a /= 0 = a
                  | isAlpha' k || k == 32 = 1
                  | otherwise = 0.001

decodeSingleByteXor :: String -> [(Float, B.ByteString)]
decodeSingleByteXor enc = take 10 . L.sortBy (comparing fst) $ scoreMapped
  where encLen = length enc
        bytes = fromHex enc
        variations = map (fixedXor bytes . B.pack . replicate encLen) [0..255]
        scoreMapped = map (\x -> (score x, x)) variations

repeatingKeyXor :: LB.ByteString -> B.ByteString -> B.ByteString
repeatingKeyXor k pt = B.pack $ B.zipWith fixedXorByte key pt
  where key = LB.toStrict . LB.take ptLen . LB.cycle $ k
        ptLen = fromIntegral . B.length $ pt

hammingDistance :: B.ByteString -> B.ByteString -> Int
hammingDistance x y = sum . map BTS.popCount $ B.zipWith fixedXorByte x y

decodeRepeatingKeyXor :: String -> [B.ByteString]
decodeRepeatingKeyXor ct = map (getResult . fst) minEditDistances
  where normEditDistance keySize
          | keySize*4 > length ct = 100000000
          | otherwise = ham
          where blocks = LS.chunksOf keySize ct
                blockHam bl1 bl2 = (fromIntegral $ hammingDistance (C8.pack bl1) (C8.pack bl2)) / fromIntegral keySize
                pairs = zip blocks $ tail blocks
                ham = (foldl (\acc (x,y) -> acc + blockHam x y) 0 pairs) / (fromIntegral $ length blocks - 1)
        editDistances = map (\x -> (x, normEditDistance x)) [2..40]
        minEditDistances = take 3 $ L.sortBy (comparing snd) editDistances
        getResult ed = B.concat . B.transpose . map snd . concat $ results
          where ctBlocks = map C8.pack . LS.chunksOf ed $ ct
                ctTranspose = B.transpose ctBlocks
                results = map (take 1 . decodeSingleByteXor . toHex) ctTranspose
