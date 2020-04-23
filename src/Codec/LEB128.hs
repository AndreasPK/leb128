{- | Encode numbers to and from bytestrings using the (S)LEB128 encoding.

     The implementation is backed by the generic algorithms defined
     in "Codec.LEB128.Generic".

-}

module Codec.LEB128
    (
    -- * Encode values as bytestring builder.
      toULEB128Builder
    , toSLEB128Builder
    -- * Encode value as bytestring.
    , toULEB128ByteString
    , toSLEB128ByteString

    -- * Encode value from bytestring.
    , fromULEB128ByteString
    , fromSLEB128ByteString
    , fromULEB128ByteStringN
    , fromSLEB128ByteStringI

    -- * Encode value from bytestring. Throws an error if not enough bytes are provided.
    , fromULEB128ByteStringUnsafe
    , fromSLEB128ByteStringUnsafe
    , fromULEB128ByteStringNUnsafe
    , fromSLEB128ByteStringIUnsafe
    )
where

import Data.Bits

import Numeric.Natural
import Data.Word
import Data.Int

import Data.ByteString.Builder.Extra (toLazyByteStringWith, safeStrategy, AllocationStrategy)
import Data.ByteString.Builder as B(Builder)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString (ByteString)

import Codec.LEB128.Constraints
import Codec.LEB128.Internal.BS as B

-- Builders

{-# SPECIALIZE toULEB128Builder :: Natural -> Builder #-}
{-# SPECIALIZE toULEB128Builder :: Word -> Builder #-}
{-# SPECIALIZE toULEB128Builder :: Word64 -> Builder #-}
{-# SPECIALIZE toULEB128Builder :: Word32 -> Builder #-}
-- | Encode values as bytestring builder.
{-# INLINEABLE toULEB128Builder #-}
toULEB128Builder :: LEB128 a => a -> B.Builder
toULEB128Builder = B.toULEB128

{-# SPECIALIZE toSLEB128Builder :: Integer -> Builder #-}
{-# SPECIALIZE toSLEB128Builder :: Int -> Builder #-}
{-# SPECIALIZE toSLEB128Builder :: Int64 -> Builder #-}
{-# SPECIALIZE toSLEB128Builder :: Int32 -> Builder #-}
{-# INLINEABLE toSLEB128Builder #-}
toSLEB128Builder :: SLEB128 a => a -> B.Builder
toSLEB128Builder = B.toSLEB128

smallChunks :: AllocationStrategy
smallChunks = safeStrategy 12 64

-- Conversion to encodede ByteString

{-# SPECIALIZE toULEB128ByteString :: Natural -> ByteString #-}
{-# SPECIALIZE toULEB128ByteString :: Word -> ByteString #-}
{-# SPECIALIZE toULEB128ByteString :: Word64 -> ByteString #-}
{-# SPECIALIZE toULEB128ByteString :: Word32 -> ByteString #-}
{-# INLINEABLE toULEB128ByteString #-}
toULEB128ByteString :: LEB128 a => a -> ByteString
toULEB128ByteString = toStrict . toLazyByteStringWith smallChunks mempty . toULEB128Builder

{-# SPECIALIZE toSLEB128ByteString :: Integer -> ByteString #-}
{-# SPECIALIZE toSLEB128ByteString :: Int -> ByteString #-}
{-# SPECIALIZE toSLEB128ByteString :: Int64 -> ByteString #-}
{-# SPECIALIZE toSLEB128ByteString :: Int32 -> ByteString #-}
{-# INLINEABLE toSLEB128ByteString #-}
toSLEB128ByteString :: SLEB128 a => a -> ByteString
toSLEB128ByteString = toStrict . toLazyByteStringWith smallChunks mempty . toSLEB128Builder

-- Conversion from encoded ByteString

{-# SPECIALIZE fromULEB128ByteString :: ByteString-> (Maybe Natural,ByteString) #-}
{-# SPECIALIZE fromULEB128ByteString :: ByteString-> (Maybe Word,ByteString) #-}
{-# SPECIALIZE fromULEB128ByteString :: ByteString-> (Maybe Word64,ByteString) #-}
{-# SPECIALIZE fromULEB128ByteString :: ByteString-> (Maybe Word32,ByteString) #-}
{-# INLINEABLE fromULEB128ByteString #-}
fromULEB128ByteString :: LEB128 a => ByteString -> (Maybe a,ByteString)
fromULEB128ByteString = B.fromULEB128

{-# SPECIALIZE fromSLEB128ByteString :: ByteString-> (Maybe Integer,ByteString) #-}
{-# SPECIALIZE fromSLEB128ByteString :: ByteString-> (Maybe Int,ByteString) #-}
{-# SPECIALIZE fromSLEB128ByteString :: ByteString-> (Maybe Int64,ByteString) #-}
{-# SPECIALIZE fromSLEB128ByteString :: ByteString-> (Maybe Int32,ByteString) #-}
{-# INLINEABLE fromSLEB128ByteString #-}
fromSLEB128ByteString :: SLEB128 a => ByteString -> (Maybe a,ByteString)
fromSLEB128ByteString = B.fromSLEB128

{-# NOINLINE fromULEB128ByteStringN #-}
fromULEB128ByteStringN :: ByteString -> (Maybe Natural,ByteString)
fromULEB128ByteStringN = B.fromULEB128

{-# NOINLINE fromSLEB128ByteStringI #-}
fromSLEB128ByteStringI :: ByteString -> (Maybe Integer,ByteString)
fromSLEB128ByteStringI = B.fromSLEB128

-- Be partial if not enough bytes are provided.

{-# SPECIALIZE fromULEB128ByteStringUnsafe :: ByteString-> (Natural,ByteString) #-}
{-# SPECIALIZE fromULEB128ByteStringUnsafe :: ByteString-> (Word,ByteString) #-}
{-# SPECIALIZE fromULEB128ByteStringUnsafe :: ByteString-> (Word64,ByteString) #-}
{-# SPECIALIZE fromULEB128ByteStringUnsafe :: ByteString-> (Word32,ByteString) #-}
{-# INLINEABLE fromULEB128ByteStringUnsafe #-}
fromULEB128ByteStringUnsafe :: LEB128 a => ByteString -> (a,ByteString)
fromULEB128ByteStringUnsafe = B.fromULEB128Unsafe

{-# SPECIALIZE fromSLEB128ByteStringUnsafe :: ByteString-> (Integer,ByteString) #-}
{-# SPECIALIZE fromSLEB128ByteStringUnsafe :: ByteString-> (Int,ByteString) #-}
{-# SPECIALIZE fromSLEB128ByteStringUnsafe :: ByteString-> (Int64,ByteString) #-}
{-# SPECIALIZE fromSLEB128ByteStringUnsafe :: ByteString-> (Int32,ByteString) #-}
{-# INLINEABLE fromSLEB128ByteStringUnsafe #-}
fromSLEB128ByteStringUnsafe :: SLEB128 a => ByteString -> (a,ByteString)
fromSLEB128ByteStringUnsafe = B.fromSLEB128Unsafe

{-# NOINLINE fromULEB128ByteStringNUnsafe #-}
fromULEB128ByteStringNUnsafe :: ByteString -> (Natural,ByteString)
fromULEB128ByteStringNUnsafe = B.fromULEB128Unsafe

{-# NOINLINE fromSLEB128ByteStringIUnsafe #-}
fromSLEB128ByteStringIUnsafe :: ByteString -> (Integer,ByteString)
fromSLEB128ByteStringIUnsafe = B.fromSLEB128Unsafe
