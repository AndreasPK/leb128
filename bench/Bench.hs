{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Criterion.Main
import GHC.Exts

-- import Data.LEB
import Codec.LEB128.List
import qualified Codec.LEB128.BS as BS
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


main = do
  let bs_1 = pack [1]
  let bs_maxBoundInt = pack $ putULEB128 (maxBound :: Int) :: ByteString
  return $! force bs_1
  return $! force bs_maxBoundInt

  defaultMain
    [   bgroup "list" [
            bgroup "unsigned"
            [   bgroup "put"
                    [   bench "w64_0_id" $ whnf (sum . (:[])) (0 :: Word64)
                    ,   bench "w64_0" $ whnf (sum . putULEB128) (0 :: Word64)
                    ,   bench "w64_1" $ whnf (sum . putULEB128) (1 :: Word64)
                    ,   bench "w64_255" $ whnf (sum . putULEB128) (255 :: Word64)
                    ,   bench "w64_max" $ nf (sum . putULEB128) (maxBound :: Word64)
                    ]
            ,   bgroup "get"
                    [   bench "w64_1  " $ whnf (fst . getULEB128 :: [Word8] -> Int) bytes_1
                    ,   bench "w64_max" $ whnf (fst . getULEB128 :: [Word8] -> Int) bytes_maxBoundInt
                    ]
            ]
        ,   bgroup "signed"
            [   bgroup "put"
                    [   bench "i64_0" $ whnf (sum . putSLEB128) (0 :: Int64)
                    ,   bench "i64_1" $ whnf (sum . putSLEB128) (1 :: Int64)
                    ,   bench "i64_255" $ whnf (sum . putSLEB128) (255 :: Int64)
                    ,   bench "i64_max" $ whnf (sum . putSLEB128) (maxBound :: Int64)
                    ]
            ,   bgroup "get"
                    [   bench "w64_1  " $ whnf (fst . getSLEB128 :: [Word8] -> Int) bytes_1
                    ,   bench "w64_max" $ whnf (fst . getSLEB128 :: [Word8] -> Int) bytes_maxBoundInt
                    ,   bench "w64_max" $ whnf (fst . getSLEB128 :: [Word8] -> Int) bytes_minBoundInt
                    ]
            ]
        ]
    ,   bgroup "bs" [
            bgroup "unsigned"
            [   bgroup "put"
                    [   bench "w64_0_id" $ nf (sum . (:[])) (0 :: Word64)
                    ,   bench "w64_0" $ nf (build_bs . BS.putULEB128) (0 :: Word64)
                    ,   bench "w64_1" $ nf (build_bs . BS.putULEB128) (1 :: Word64)
                    ,   bench "w64_255" $ nf (build_bs . BS.putULEB128) (255 :: Word64)
                    ,   bench "w64_max" $ nf (build_bs . BS.putULEB128) (maxBound :: Word64)
                    ]
            ,   bgroup "get"
                    [   bench "w64_1  " $ whnf (fst . BS.getULEB128 :: ByteString -> Int) bs_1
                    ,   bench "w64_max" $ whnf (fst . BS.getULEB128 :: ByteString -> Int) bs_maxBoundInt
                    ]
            ]
        ,   bgroup "signed"
            [   bgroup "put"
                    [   bench "i64_0" $ nf (build_bs . BS.putSLEB128) (0 :: Int64)
                    ,   bench "i64_1" $ nf (build_bs . BS.putSLEB128) (1 :: Int64)
                    ,   bench "i64_255" $ nf (build_bs . BS.putSLEB128) (255 :: Int64)
                    ,   bench "i64_max" $ nf (build_bs . BS.putSLEB128) (maxBound :: Int64)
                    ]
            ,   bgroup "get"
                    [   bench "w64_1  " $ nf (fst . BS.getSLEB128 :: ByteString -> Int) bs_1
                    ,   bench "w64_max" $ nf (fst . BS.getSLEB128 :: ByteString -> Int) bs_maxBoundInt
                    ]
            ]
        ]
    ]

