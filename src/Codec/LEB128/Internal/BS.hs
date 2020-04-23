{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
    Module      : Codec.LEB128.Internal.BS
    Description : Encode values via (S)LEB128 using bytestring.
    Copyright   : (c) Andreas Klebinger 2020
    License     : BSD3
    Maintainer  : Andreas Klebinger
    Portability : GHC >= 7.10

     This module specializes the generic algorithms defined
     in "Codec.LEB128.Generic" to use ByteString as byte
     sequence representation.
-}

module Codec.LEB128.Internal.BS
  ( fromULEB128
  , fromSLEB128
  , fromULEB128Unsafe
  , fromSLEB128Unsafe
  , toULEB128
  , toSLEB128
  )
where

import Data.Word (Word8)
import Data.Maybe
import GHC.Magic
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as BS

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad

import Codec.LEB128.Constraints
import Codec.LEB128.Generic as G

-- | Encode a unsigned value as bytestring builder in LEB128 encoding.
{-# INLINEABLE toULEB128 #-}
toULEB128 :: LEB128 a => a -> B.Builder
toULEB128 = (inline G.encodeLEB128) (B.word8)

-- | Encode a signed value as bytestring builder in SLEB128 encoding.
{-# INLINEABLE toSLEB128 #-}
toSLEB128 :: SLEB128 a => a -> B.Builder
toSLEB128 = (inline G.encodeSLEB128) (B.word8)

type UnsafeByteProvider = State (BS.ByteString)

{-# INLINABLE getByteUnsafe #-}
getByteUnsafe :: UnsafeByteProvider Word8
getByteUnsafe = do
    (bs) <- get
    let (!byte,!bs') = fromMaybe (error "Not enough bytes") $ BS.uncons bs
    put $! bs'
    return byte

{-# INLINABLE fromULEB128Unsafe #-}
-- | Decode a value in unsigned LEB128 encoding and return remaining bytes.
fromULEB128Unsafe :: LEB128 a => BS.ByteString -> (a,BS.ByteString)
fromULEB128Unsafe bytes = runState
                    ((inline G.decodeLEB128) getByteUnsafe)
                    bytes

{-# INLINABLE fromSLEB128Unsafe #-}
-- | Decode a value in (signed) SLEB128 encoding and return remaining bytes.
fromSLEB128Unsafe :: SLEB128 a => BS.ByteString -> (a,BS.ByteString)
fromSLEB128Unsafe bytes = runState
                    ((inline G.decodeSLEB128) getByteUnsafe)
                    bytes

type ByteProvider a = MaybeT (State BS.ByteString) a

{-# INLINE runByteProvider #-}
runByteProvider :: ByteProvider a -> BS.ByteString -> (Maybe a, BS.ByteString)
runByteProvider action = runState (runMaybeT action)

{-# INLINE liftMaybe #-}
liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

{-# INLINABLE getByte #-}
getByte :: (ByteProvider Word8)
getByte = do
    (bs) <- lift get
    (!byte,!bs') <- liftMaybe (BS.uncons bs)
    lift $! put $! bs'
    return byte

{-# INLINABLE fromULEB128 #-}
-- | Decode a value in unsigned LEB128 encoding and return remaining bytes.
fromULEB128 :: forall a. LEB128 a => BS.ByteString -> (Maybe a,BS.ByteString)
fromULEB128 =
    let decode = (inline G.decodeLEB128) getByte :: MaybeT (State BS.ByteString) a
    in runByteProvider decode

{-# INLINABLE fromSLEB128 #-}
-- | Decode a value in (signed) SLEB128 encoding and return remaining bytes.
fromSLEB128 :: forall a. SLEB128 a => BS.ByteString -> (Maybe a,BS.ByteString)
fromSLEB128 =
    let decode = (inline G.decodeSLEB128) getByte :: MaybeT (State BS.ByteString) a
    in runByteProvider decode


