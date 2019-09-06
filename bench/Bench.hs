{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-idinfo
    -dsuppress-module-prefixes -ddump-cmm -fproc-alignment=64 -ddump-stg #-}

module Main where

import Criterion.Main
import GHC.Exts

-- import Data.LEB
import Data.LEB.List
import Data.Int
import Data.Word

main = do
  let b_small = [122]
  let b_large = [255,255,255,255,16]

  defaultMain [
    bgroup "unsigned" [
        -- bgroup "get" [
        --     bench "small" $ whnf getULEB128_Int b_small,
        --     bench "large" $ whnf getULEB128_Int b_large
        --     ],
        bgroup "put" [
            -- bench "w32_0" $ whnf (sum . putULEB128) (0 :: Word32),
            -- bench "w32_1" $ whnf (sum . putULEB128) (1 :: Word32),
            -- bench "w32_255" $ whnf (sum . putULEB128) (255 :: Word32),
            bench "w64_max" $
                whnf    (( getULEB128 :: [Word8] -> Word64) .
                            putULEB128_W64)
                        (maxBound :: Word64)

            ]
        ]
    ]

-- {-# NOINLINE getULEB128_Int #-}
-- getULEB128_Int :: [Word8] -> Int
-- getULEB128_Int = getULEB128

-- {-# NOINLINE putULEB128_W64 #-}
putULEB128_W64 :: Word64 -> [Word8]
putULEB128_W64 = putULEB128

foo :: Int -> Word8
foo !w = 1 + fromIntegral w

bar :: Int -> [Word8]
bar !x = [fromIntegral x]