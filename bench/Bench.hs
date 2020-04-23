{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Criterion.Main
import GHC.Exts

-- import Data.LEB
import Codec.LEB128.List
import qualified Codec.LEB128 as BS
import Data.Int
import Data.Word
import Data.ByteString (pack, ByteString)
import Control.DeepSeq
import Data.ByteString.Builder
import Data.ByteString.Builder.Internal (toLazyByteStringWith, safeStrategy, untrimmedStrategy)
import Data.ByteString.Lazy (toStrict)

bytes_maxBoundInt = [255,255,255,255,255,255,255,255,127] :: [Word8]
bytes_minBoundInt = [128,128,128,128,128,128,128,128,128,127] :: [Word8]
bytes_1 = [1] :: [Word8]

-- Going through the BS.Builder interface comes with a good deal
-- of overhead. Decoding a large integer can take as much as 4x
-- as long as decoding to a list directly for example.
-- However in exchange we get a fair bit of flexibility.
{-# INLINE build_bs #-}
build_bs :: Builder -> ByteString
build_bs = toStrict .
    (toLazyByteStringWith
        (untrimmedStrategy 11 123)
        mempty
    )

benchPut :: NFData a => (Word64 -> a) -> Benchmark
benchPut encoder = bgroup "putOutlined"
    [   bench "w64_0" $ nf encoder (0 :: Word64)
    ,   bench "w64_1" $ nf encoder (1 :: Word64)
    ,   bench "w64_255" $ nf encoder (255 :: Word64)
    ,   bench "w64_max" $ nf encoder (maxBound :: Word64)
    ]

benchGetBS :: NFData b => String -> (ByteString -> b) -> Benchmark
benchGetBS name decoder =
    bgroup name
        [   bench "w64_1  " $ nf (decoder) bs_1
        ,   bench "w64_max" $ nf (decoder) bs_maxBoundInt
        ]

bs_1 :: ByteString
bs_1 = pack [1]
bs_maxBoundInt :: ByteString
bs_maxBoundInt = pack $ toSLEB128 (maxBound :: Int) :: ByteString

main = do
  return $! force bs_1
  return $! force bs_maxBoundInt

  defaultMain
    [   bench "benchmark_overhead" $ nf (:[]) (0 :: Word64)
    ,   bgroup "list" [
            bgroup "unsigned"
            [   bgroup "put"
                    [   bench "w64_0" $ nf (toULEB128) (0 :: Word64)
                    ,   bench "w64_1" $ nf (toULEB128) (1 :: Word64)
                    ,   bench "w64_255" $ nf (toULEB128) (255 :: Word64)
                    ,   bench "w64_max" $ nf (toULEB128) (maxBound :: Word64)
                    ]
            ,   bgroup "get"
                    [   bench "w64_1  " $ whnf (fst . fromULEB128 :: [Word8] -> Word) bytes_1
                    ,   bench "w64_max" $ whnf (fst . fromULEB128 :: [Word8] -> Word) bytes_maxBoundInt
                    ]
            ]
        ,   bgroup "signed"
            [   bgroup "put"
                    [   bench "i64_0" $ whnf (sum . toSLEB128) (0 :: Int64)
                    ,   bench "i64_1" $ whnf (sum . toSLEB128) (1 :: Int64)
                    ,   bench "i64_255" $ whnf (sum . toSLEB128) (255 :: Int64)
                    ,   bench "i64_max" $ whnf (sum . toSLEB128) (maxBound :: Int64)
                    ]
            ,   bgroup "get"
                    [   bench "w64_1  " $ whnf (fst . fromSLEB128 :: [Word8] -> Int) bytes_1
                    ,   bench "w64_max" $ whnf (fst . fromSLEB128 :: [Word8] -> Int) bytes_maxBoundInt
                    ,   bench "w64_max" $ whnf (fst . fromSLEB128 :: [Word8] -> Int) bytes_minBoundInt
                    ]
            ]
        ]
    ,   bgroup "bs"
            [   benchPut BS.toULEB128ByteString
            ,   benchPut BS.toULEB128ByteString
            ,   benchGetBS "getInt" (BS.fromSLEB128ByteString :: ByteString -> (Maybe Int,ByteString))
            ,   benchGetBS "getInteger" (BS.fromSLEB128ByteString :: ByteString -> (Maybe Integer,ByteString))
            ,   benchGetBS "getIntUnsafe" (BS.fromSLEB128ByteStringUnsafe :: ByteString -> (Int,ByteString))
            ,   benchGetBS "getIntegerUnsafe" (BS.fromSLEB128ByteStringUnsafe :: ByteString -> (Integer,ByteString))
            ]

    ]

