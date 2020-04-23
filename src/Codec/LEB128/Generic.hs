{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}


{- |
    Module      : Codec.LEB128.Generic
    Description : Encode values via (S)LEB128
    Copyright   : (c) Andreas Klebinger 2020
    License     : BSD3
    Maintainer  : Andreas Klebinger
    Portability : GHC >= 7.10

     This module provides a generic interface over the encoding
     and decoding algorithm. It can be instantiated to a wide
     variate of types.

     Instantiations based on bytestring and lists are provided in the
     "Codec.LEB128.List" and "Codec.LEB128.Internal.BS" modules.

     Size checks for inputs or output types are not performed by default.
     However they can be included in the put/get functions if desired.

-}

module Codec.LEB128.Generic
  (
    -- * Generic encoding functions
    encodeLEB128
  , encodeSLEB128
    -- * Generic decoding functions
  , decodeLEB128
  , decodeSLEB128)
where

-- import Control.Applicative
import Data.Bits ((.|.), Bits, unsafeShiftR, unsafeShiftL,
                  testBit, clearBit, setBit, bit)
import Data.Word
import Data.Monoid ((<>))
import Prelude hiding ((<>))
import GHC.Magic

import Codec.LEB128.Constraints

-- | LEB128-encode a unsigned value into a sequence of bytes.
--
-- For example to encode a integer into a list of words you might use.
--
-- > encodeLEB128 pure :: Integer -> [Word8]
--
-- To do the same using a serialization library like bytestrings builder:
--
-- > encodeLEB128 (B.word8)
--
-- For performance reasons it can be important to make sure @encodeLEB128@
-- is sufficiently specialized. One way to achieve this is to force inlining
-- using the @inline@ function from GHC.Magic (defined in the ghc-prim package).
-- For an efficient example generic over the value type this gives us for lists:
--
-- @
--    toULEB128 :: (Integral a, Bits a) => a -> [Word8]
--    toULEB128 = (inline G.encodeLEB128) pure
-- @
--
-- Results are undefined for negative numbers.
{-# INLINE encodeLEB128 #-}
encodeLEB128 :: forall a m. (Monoid m, LEB128 a) => (Word8 -> m) -> a -> m
encodeLEB128 !putWord8 = go
  where
    go !i
      | i <= 127
      = (inline putWord8) $! (fromIntegral i :: Word8)
      | otherwise =
        -- bit 7 (8th bit) indicates more to come.
        let !byte = (setBit (fromIntegral i) 7)
        in (inline putWord8) byte <> go (i `unsafeShiftR` 7)

-- | SLEB128-encodes an singed value into a sequence of bytes.
--
-- Works the same as @encodeLEB128@ but supports negative values.
{-# INLINE encodeSLEB128 #-}
encodeSLEB128 :: forall a m. (Monoid m, SLEB128 a) => (Word8 -> m) -> a -> m
encodeSLEB128 putWord8 = go
  where
    go val = do
        let !byte = fromIntegral (clearBit val 7) :: Word8
        let !val' = val `unsafeShiftR` 7
        let !signBit = testBit byte 6
        let !done =
                -- Unsigned value, val' == 0 and last value can
                -- be discriminated from a negative number.
                (val' == 0 && not signBit) ||
                -- Signed value,
                (val' == -1 && signBit)
        let !byte' = if done then byte else setBit byte 7
        putWord8 byte' <> if done then mempty else go val'

-- | LEB128-decodes a unsigned value given a monadic way to request bytes.
--
-- For example a implementation over a state monad might look like:
--
-- > execState . decodeLEB128 getByte
--
-- This pattern is used by the bytestring based decoder in this package.
-- See there for a complete example.
{-# INLINE decodeLEB128 #-}
decodeLEB128 :: forall a m. (Monad m, LEB128 a) => m Word8 -> m a
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

-- | SLEB128-decodes a unsigned number given a monadic way to request bytes.
--
-- Same as decodeLEB128 but for the signed encoding.
{-# INLINE decodeSLEB128 #-}
decodeSLEB128 :: forall a m. (Monad m, SLEB128 a) => m Word8 -> m a
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
