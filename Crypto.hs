module Crypto
(
bsSplit
,toHex
,fromHex
,toBase64
,fromBase64
,fixedXor
,fixedXorByte
,decodeSingleByteXor
,repeatingKeyXor
,hammingDistance
,decodeRepeatingKeyXor
,aesKey
,aesECBEncrypt
,aesECBDecrypt
,isAESInECB
,pkcs7Pad
,aesCBCEncrypt
,aesCBCDecrypt
,aesRandIV
,aesEncryptRandom
,aesByteAtATimeECBEncryptSimple
,aesByteAtATimeECBEncryptHarder
,getBlockSizeSimple
,findPrefixLength
,aesByteAtATimeECBDecrypt
,aesByteAtATimeECBEncryptHarderWrapper
,cbcBitFlipAttackEnc
,cbcBitFlipAttackDec
,randCBCEncrypt
,randCBCDecrypt
,crackCBCPaddingOracle
,constructProfile
,profileEncode
,profileDecode
,getProfile
,remakeProfile
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base16 as B16

import qualified Data.ByteString.Char8 as C8

import qualified Data.Word as W

import qualified Codec.Encryption.AES as AES
import qualified Data.LargeWord as LGW

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as LS

import qualified Data.Map as M
import qualified Numeric as N
import qualified Data.Bits as BTS
import qualified Text.Printf as TPF
import Data.Ord (comparing)
import System.Random (randomRIO)
import System.Entropy (getEntropy)
import Control.Monad (guard)

type EncryptionAlgorithm = LGW.Word128 -> B.ByteString -> B.ByteString

data User = User {  email :: String
                  , uid :: Int
                  , role :: String
                  } deriving (Show)

group :: String -> [(String, String)]
group s = L.foldl (\acc x -> let (k:v:_) = LS.splitOn "=" x in (k,v):acc ) [] $ LS.splitOn "&" s

profileEncode :: User -> String
profileEncode u = "email=" ++ email u ++ "&uid=" ++ show (uid u) ++ "&role=" ++ role u

profileDecode :: String -> Maybe User
profileDecode s = do
  let grps = group s
      lk = flip lookup grps
  e <- lk "email"
  u <- lk "uid"
  r <- lk "role"
  return User {email=e, uid=read u, role=r}


constructProfile :: String -> User
constructProfile em = User {email=parsed, uid=10, role="user"}
  where parsed = (filter . flip notElem) "&=" em

getProfile :: LGW.Word128 -> String -> B.ByteString
getProfile k p = aesECBEncrypt k . C8.pack . profileEncode . constructProfile $ p

remakeProfile :: LGW.Word128 -> B.ByteString -> B.ByteString
remakeProfile k ct = aesECBDecrypt k ct

charFreqs :: M.Map W.Word8 Float
charFreqs = M.fromList . map (\(x, y) -> (fromIntegral . C.ord $ x, y)) $ [('E', 0.1202), ('T', 0.091), ('A', 0.0812), ('O', 0.0768), ('I', 0.0731), ('N', 0.0695), ('S', 0.06280000000000001), ('R', 0.0602), ('H', 0.0592), ('D', 0.0432), ('L', 0.0398), ('U', 0.0288), ('C', 0.0271), ('M', 0.026099999999999998), ('F', 0.023), ('Y', 0.021099999999999997), ('W', 0.0209), ('G', 0.0203), ('P', 0.0182), ('B', 0.0149), ('V', 0.0111), ('K', 0.0069), ('X', 0.0017000000000000001), ('Q', 0.0011), ('J', 0.001), ('Z', 0.0007000000000000001)]

asciiToUpper :: W.Word8 -> W.Word8
asciiToUpper x = if x `elem` [97..122] then x-32 else x

isAlpha' :: W.Word8 -> Bool
isAlpha' x = x `elem` [97..122]++[65..90]

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
fixedXorByte = BTS.xor

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

bsSplit :: Int -> B.ByteString -> [B.ByteString]
bsSplit n bs
  | B.length bs == 0 = []
  | B.length bs < n = [bs]
  | otherwise = B.take n bs : bsSplit n (B.drop n bs)

fromBytes :: (Num a, BTS.Bits a) => [a] -> a
fromBytes input =
    let dofb accum [] = accum
        dofb accum (x:xs) = dofb ((BTS.shiftL accum 8) BTS..|. x) xs
        in
        dofb 0 input

blockWord8sIn64 :: [W.Word8] -> [[W.Word8]]
blockWord8sIn64 =
   L.unfoldr g
   where
      g [] = Nothing
      g xs = Just (L.splitAt 8 xs)

getWord64s :: [W.Word8] -> [W.Word64]
getWord64s =
   map fromBytes . map (map fromIntegral) .  blockWord8sIn64

getWord128 :: W.Word64 -> W.Word64 -> LGW.Word128
getWord128 x y = (BTS.shiftL (fromIntegral x) 64) BTS..|. (fromIntegral y)

fromWord128 :: LGW.Word128 -> [W.Word8]
fromWord128 bl = reverse $ helper (fromIntegral bl :: Integer) 16
  where helper _ 0 = []
        helper b c =
          let w8 = fromIntegral b :: W.Word8
              shifted = BTS.shiftR b 8
              in
              w8 : helper shifted (c-1)

byteStringToWord128s :: B.ByteString -> [LGW.Word128]
byteStringToWord128s bs =
  let w64s = getWord64s . B.unpack $ bs
      w64pairs = helper w64s
        where helper [] = []
              helper (a:b:cs) = (a,b) : helper cs
              helper _ = error "invalid length"
      in
      map (uncurry getWord128) w64pairs

aesKey :: B.ByteString -> LGW.Word128
aesKey k
 | B.length k == 16 =
    let [k1, k2] = getWord64s . B.unpack $ k
        in
        getWord128 k1 k2
 | otherwise = error "invalid key size"

pkcs7Pad :: Int -> B.ByteString -> B.ByteString
pkcs7Pad n b = B.append b $ B.replicate ap $ fromIntegral ap
 where ap = n - (B.length b `mod` n)

aesECBEncrypt :: LGW.Word128 -> B.ByteString -> B.ByteString
aesECBEncrypt k pt =
  let ptBlocks = byteStringToWord128s . pkcs7Pad 16 $ pt
      in
      B.pack . L.concatMap fromWord128 $ map (AES.encrypt k) ptBlocks

aesECBDecrypt :: LGW.Word128 -> B.ByteString -> B.ByteString
aesECBDecrypt k ct =
  let ctBlocks = byteStringToWord128s $ ct
      in
      B.pack . L.concatMap fromWord128 $ map (AES.decrypt k) ctBlocks

isAESInECB :: B.ByteString -> Bool
isAESInECB ct =
  let (bl:bls) = byteStringToWord128s ct
      helper _ [] = False
      helper b bs@(c:ds)
        | b `elem` bs = True
        | otherwise = helper c ds
      in
      helper bl bls

aesCBCEncrypt :: LGW.Word128 -> LGW.Word128 -> B.ByteString -> B.ByteString
aesCBCEncrypt k iv pt =
  B.pack . L.concatMap fromWord128 $ helper iv ptBlocks
    where ptBlocks = byteStringToWord128s . pkcs7Pad 16 $ pt
          helper _ [] = []
          helper lst (p:ps) = pEnc : helper pEnc ps
            where pEnc = AES.encrypt k (p `BTS.xor` lst)

aesCBCDecrypt :: LGW.Word128 -> LGW.Word128 -> B.ByteString -> B.ByteString
aesCBCDecrypt k iv ct =
  B.pack . L.concatMap fromWord128 $ helper iv ctBlocks
    where ctBlocks = byteStringToWord128s ct
          helper _ [] = []
          helper lst (c:cs) = cDec : helper c cs
            where cDec = AES.decrypt k c `BTS.xor` lst

aesRandIV :: IO LGW.Word128
aesRandIV = do
  b <- getEntropy 16
  return $ aesKey b

aesEncryptRandom :: B.ByteString -> IO B.ByteString
aesEncryptRandom pt = do
  key <- aesRandIV
  iv <- aesRandIV
  ap <- randomRIO(5,10) :: IO Int
  ap1 <- getEntropy ap
  ap2 <- getEntropy ap
  mode <- randomRIO (1,2) :: IO Int
  let p = B.append ap1 $ B.append pt ap2
  -- print mode -- for testing
  if mode == 1 then
    -- ecb
    return $ aesECBEncrypt key p
  else
    -- cbc
    return $ aesCBCEncrypt key iv p

aesByteAtATimeECBEncryptSimple :: LGW.Word128 -> B.ByteString -> B.ByteString
aesByteAtATimeECBEncryptSimple k pt =
  aesECBEncrypt k x
  where secret = fromBase64 "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"
        x = pt `B.append` secret

aesByteAtATimeECBEncryptHarder :: LGW.Word128 -> B.ByteString -> B.ByteString
aesByteAtATimeECBEncryptHarder k pt =
  aesECBEncrypt k x
  where secret = fromBase64 "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"
        prefix = fromBase64 "8RHAQQe8QtDTCOcq23BquaBuwZOsfUDX5t19Gq3Ie72vbdm7EGJ5yglF7AfYxzO3iKojv1IE"
        x = prefix `B.append` pt `B.append` secret

aesByteAtATimeECBEncryptHarderWrapper :: Int -> Int -> LGW.Word128 -> B.ByteString -> B.ByteString
aesByteAtATimeECBEncryptHarderWrapper initPad dropLen k pt =
  B.drop dropLen $ aesByteAtATimeECBEncryptHarder k $ B.replicate initPad 65 `B.append` pt

getBlockSizeSimple :: EncryptionAlgorithm -> LGW.Word128 -> Int
getBlockSizeSimple oracle k =
  helper 0 0
    where helper c l
            | c == 0 = helper 1 encLen
            | encLen /= l = encLen - l
            | otherwise = helper (c+1) encLen
              where encLen = B.length . oracle k $ B.replicate c 65

aesByteAtATimeECBDecrypt :: EncryptionAlgorithm -> Int -> LGW.Word128 -> B.ByteString
aesByteAtATimeECBDecrypt enc blSz k = helper 1 B.empty (blSz-1)
  where secretLen = B.length $ enc k B.empty
        matchBlock target less1 = do
          x <- [0..255]
          let r = B.take blSz . enc k $ B.snoc less1 x
          guard (r == target)
          return x
        helper blc dbs (-1) = helper (blc+1) dbs (blSz-1)
        helper blc dbs bc
          | null dec  = dbs
          | otherwise = helper blc (dbs `B.snoc` (head dec)) $ bc-1
            where pad = B.replicate bc 65
                  tar = B.take blSz . dropFunc . enc k $ pad
                  dec = matchBlock tar $ dropFunc $ pad `B.append` dbs
                  dropFunc = B.drop (blSz * (blc-1))

findPrefixLength :: EncryptionAlgorithm -> LGW.Word128 -> (Int, Int)
findPrefixLength enc k = (pad, helper' B.empty repeatedBlock B.empty)
  where blSz = getBlockSizeSimple enc k
        pad  = helper 0
        helper c
          | isAESInECB ct = c `mod` blSz
          | otherwise     = helper $ c+1
          where ct = enc k $ B.replicate c 65
        repeatedBlock = enc k $ B.replicate (pad+blSz*2) 65
        helper' processed leftover previous
          | previous == B.empty = helper' processed tl hdr
          | previous == hdr     = B.length processed
          | otherwise           = helper' pr tl hdr
          where hdr = B.take blSz leftover
                tl  = B.drop blSz leftover
                pr  = hdr `B.append` processed

validPadding :: B.ByteString -> Bool
validPadding s =
  count still $ fromIntegral lst - 1
  where split str = (B.last str, B.init str)
        (lst, still) = split s
        count _  0 = True
        count st r = if l == lst
                     then count ls $ r-1
                     else False
                     where (l, ls) = split st

quote :: B.ByteString -> B.ByteString
quote = B.pack . reverse . helper [] . B.unpack
  where helper acc (w:ws) = case w of
                              59 -> helper (66:51:37:acc) ws
                              61 -> helper (68:51:37:acc) ws
                              x  -> helper (x:acc) ws
        helper acc _      = acc

cbcBitFlipAttackEnc :: LGW.Word128 -> B.ByteString -> B.ByteString
cbcBitFlipAttackEnc k pt = aesCBCEncrypt k k modified
  where pre      = C8.pack "comment1=cooking%20MCs;userdata="
        suf      = C8.pack ";comment2=%20like%20a%20pound%20of%20bacon"
        modified = pre `B.append` pt `B.append` suf

cbcBitFlipAttackDec :: LGW.Word128 -> B.ByteString -> Bool
cbcBitFlipAttackDec k ct = (C8.pack ";admin=true;") `B.isInfixOf` dec
  where dec = aesCBCDecrypt k k ct

randCBCEncrypt :: IO (LGW.Word128, B.ByteString)
randCBCEncrypt = do
  k' <- getEntropy 16
  n  <- randomRIO (0, 9) :: IO Int
  return $ (aesKey k', helper k' n)
  where helper k' n = aesCBCEncrypt k k $ head random
          where k      = aesKey k'
                random = drop n $ map fromBase64 [ "MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc="
                                                 , "MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic="
                                                 , "MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw=="
                                                 , "MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg=="
                                                 , "MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl"
                                                 , "MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA=="
                                                 , "MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw=="
                                                 , "MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8="
                                                 , "MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g="
                                                 , "MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93"
                                                 ]

randCBCDecrypt :: LGW.Word128 -> B.ByteString -> Bool
randCBCDecrypt k ct = validPadding dec
  where dec = aesCBCDecrypt k k ct

{-crackCBCPaddingOracle :: LGW.Word128 -> B.ByteString -> B.ByteString-}
crackCBCPaddingOracle k ct = foldl B.append B.empty $ crackWhole [] blocks
  where iv     = B.pack . fromWord128 $ k
        blocks = bsSplit 16 $ iv `B.append` ct
        crackWhole acc (a:b:cs) = crackWhole (acc++[bl]) (b:cs)
          where bl = crackBlock a b B.empty B.empty 15
        crackWhole acc _        = acc
        crackBlock _    _   _   acc (-1) = acc
        crackBlock rpre suf int acc pad  = crackBlock rpre suf (ib `B.cons` int) (pb `B.cons` acc) (pad-1)
          where rnd      = B.pack [15,149,199,91,162,248,71,76,133,92,139,27,204,236,221,189] -- just some random bytes
                pkPad    = 16-pad
                iPad     = fromIntegral pad
                mod      = B.map (`BTS.xor` pkPad) int
                ib       = pkPad `BTS.xor` head getByte
                pb       = (B.head . B.drop iPad) rpre `BTS.xor` ib
                getByte  = do
                  x <- [0..255]
                  let ct = B.take iPad rnd `B.append` B.cons x mod `B.append` suf
                  guard (randCBCDecrypt k ct)
                  return x

w64LittleEndian :: W.Word64 -> W.Word64
w64LittleEndian w = (fromIntegral w :: W.Word32)

aesCTR :: LGW.Word128 -> LGW.Word128o

-- left half  = 18446744069414584320
-- right half = 4294967295
