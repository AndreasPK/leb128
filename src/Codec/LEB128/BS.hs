{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}

{- | Encode numbers to and from variable length byte lists
     using the (S)LEB128 encoding.

     Specializations for regular Int/Word types are provided.
     Using other types is possible but might not perform as expected.

     The implementation is backed by the generic algorithms defined
     in "Codec.LEB128.Generic".
-}

module Codec.LEB128.BS
  ( getULEB128
  , getSLEB128
  , putULEB128
  , putSLEB128
  )
where

import Data.Bits
import Data.Word
import Data.Int
import Data.List
import Data.Maybe
import GHC.Magic
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as BS
import Control.Monad.Trans.State.Strict

import Codec.LEB128.Generic as G

-- | Encode a unsigned value as bytestring builder in LEB128 encoding.
{-# INLINEABLE putULEB128 #-}
putULEB128 :: (Integral a, Bits a) => a -> B.Builder
putULEB128 = (inline G.encodeLEB128) (B.word8)

-- | Encode a signed value as bytestring builder in SLEB128 encoding.
{-# INLINEABLE putSLEB128 #-}
putSLEB128 :: (Integral a, Bits a) => a -> B.Builder
putSLEB128 = (inline G.encodeSLEB128) (B.word8)

type ByteProvider = State (BS.ByteString)

{-# INLINABLE getByte #-}
getByte :: ByteProvider Word8
getByte = do
    (bs) <- get
    let (!byte,!bs') = fromMaybe (error "Not enough bytes") $ BS.uncons bs
    put $! bs'
    return byte

-- | Decode a value in unsigned LEB128 encoding and return remaining bytes.
getULEB128 :: (Integral a, Bits a) => BS.ByteString -> (a,BS.ByteString)
getULEB128 bytes = runState
                    ((inline G.decodeLEB128) getByte)
                    bytes

-- | Decode a value in (signed) SLEB128 encoding and return remaining bytes.
getSLEB128 :: (Integral a, Bits a) => BS.ByteString -> (a,BS.ByteString)
getSLEB128 bytes = runState
                    ((inline G.decodeSLEB128) getByte)
                    bytes

