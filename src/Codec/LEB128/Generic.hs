{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Codec.LEB128.Generic
  (
    -- * Generic encoding functions
    encodeLEB128
  , encodeSLEB128
    -- * Generic decoding functions
  , decodeLEB128
  , decodeSLEB128)
where

import Control.Applicative
import Data.Bits
import Data.Word
import Data.Monoid ((<>))
import Prelude hiding ((<>))

import GHC.Magic

-- | LEB128-encode a unsigned number into a sequence of bytes.
-- Undefined for negative numbers.
{-# INLINE encodeLEB128 #-}
encodeLEB128 :: forall a m. (Monoid m, Integral a, Bits a) => (Word8 -> m) -> a -> m
encodeLEB128 !putWord8 x = go x
  -- | x < 0 = error "Can't encode negative numbers in unsigned LEB format."
  where
    go !i
      | i <= 127
      = (inline putWord8) $! (fromIntegral i :: Word8)
      | otherwise =
        -- bit 7 (8th bit) indicates more to come.
        let !byte = (setBit (fromIntegral i) 7)
        in (inline putWord8) byte <> go (i `unsafeShiftR` 7)

-- | SLEB128-encodes an singed number into a sequence of bytes.
{-# INLINE encodeSLEB128 #-}
encodeSLEB128 :: forall a m. (Monoid m, Integral a, Bits a) => (Word8 -> m) -> a -> m
encodeSLEB128 putWord8 = go
  where
    go val = do
        let !byte = fromIntegral (clearBit val 7) :: Word8
        let !val' = val `unsafeShiftR` 7
        let !signBit = testBit byte 6
        let !done =
                -- Unsigned value, val' == 0 and and last value can
                -- be discriminated from a negative number.
                (val' == 0 && not signBit) ||
                -- Signed value,
                (val' == -1 && signBit)
        let !byte' = if done then byte else setBit byte 7
        putWord8 byte' <> if done then mempty else go val'

-- | LEB128-decodes a natural number via @cereal@
{-# INLINE decodeLEB128 #-}
decodeLEB128 :: forall a m. (Monad m, Integral a, Bits a) => m Word8 -> m a
decodeLEB128 getWord8 = go 0 0
  where
    go :: Int -> a -> m a
    go !shift !w = do
        byte <- getWord8
        let !byteVal = fromIntegral (clearBit byte 7)
        let !hasMore = testBit byte 7
        let !val = w .|. (byteVal `unsafeShiftL` shift)
        let !shift' = shift+7
        if hasMore
            then go shift' val
            else return $! val

-- | SLEB128-decodes an integer via @cereal@
{-# INLINE decodeSLEB128 #-}
decodeSLEB128 :: forall a m. (Monad m, Integral a, Bits a) => m Word8 -> m a
decodeSLEB128 getWord8 = go 0 0
  where
    go :: Int -> a -> m a
    go !shift !w = do
        byte <- getWord8 :: m Word8
        let !byteVal = fromIntegral (clearBit byte 7)
        let !hasMore = testBit byte 7
        let !val = w .|. (byteVal `unsafeShiftL` shift)
        let !shift' = shift+7
        if hasMore
            then go shift' val
            else do
                let !signed = testBit byte 6
                if signed
                then pure $! val - bit shift'
                else pure $! val
