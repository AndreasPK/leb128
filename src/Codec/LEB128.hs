module Codec.LEB128
    ( toULEB128Builder
    , toSLEB128Builder
    , toULEB128ByteString
    , toSLEB128ByteString
    , fromULEB128ByteString
    , fromSLEB128ByteString
    , fromULEB128ByteStringN
    , fromSLEB128ByteStringI
    )
where

import Data.Bits
import Numeric.Natural
import Data.Word

import Data.ByteString.Builder.Extra (toLazyByteStringWith, safeStrategy, AllocationStrategy)
import Data.ByteString.Builder as B(Builder)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString (ByteString)

import Codec.LEB128.BS as B

-- Builders

-- | Encode values as bytestring builder.
{-# INLINEABLE toULEB128Builder #-}
toULEB128Builder :: (Integral a, Bits a) => a -> B.Builder
toULEB128Builder = B.putULEB128

{-# INLINEABLE toSLEB128Builder #-}
toSLEB128Builder :: (Integral a, Bits a) => a -> B.Builder
toSLEB128Builder = B.putSLEB128

smallChunks :: AllocationStrategy
smallChunks = safeStrategy 12 64

-- Conversion to encodede ByteString

{-# INLINEABLE toULEB128ByteString #-}
toULEB128ByteString :: (Integral a, Bits a) => a -> ByteString
toULEB128ByteString = toStrict . toLazyByteStringWith smallChunks mempty . toULEB128Builder

{-# INLINEABLE toSLEB128ByteString #-}
toSLEB128ByteString :: (Integral a, Bits a) => a -> ByteString
toSLEB128ByteString = toStrict . toLazyByteStringWith smallChunks mempty . toSLEB128Builder

{-# INLINEABLE fromULEB128ByteString #-}
fromULEB128ByteString :: (Integral a, Bits a) => ByteString -> (a,ByteString)
fromULEB128ByteString = B.getULEB128

-- Conversion from encodede ByteString

{-# INLINEABLE fromSLEB128ByteString #-}
fromSLEB128ByteString :: (Integral a, Bits a) => ByteString -> (a,ByteString)
fromSLEB128ByteString = B.getSLEB128

{-# NOINLINE fromULEB128ByteStringN #-}
fromULEB128ByteStringN :: ByteString -> (Natural,ByteString)
fromULEB128ByteStringN = B.getULEB128

{-# NOINLINE fromSLEB128ByteStringI #-}
fromSLEB128ByteStringI :: ByteString -> (Integer,ByteString)
fromSLEB128ByteStringI = B.getSLEB128
