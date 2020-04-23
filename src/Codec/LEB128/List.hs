{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
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

module Codec.LEB128.List
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
import GHC.Magic

import Control.Monad.Trans.State.Strict

import Codec.LEB128.Generic as G

-- | Encode a unsigned value in LEB128.
{-# INLINEABLE putULEB128 #-}
putULEB128 :: (Integral a, Bits a) => a -> [Word8]
putULEB128 = (inline G.encodeLEB128) pure

-- | Encode a signed value in __S__LEB128.
{-# INLINEABLE putSLEB128 #-}
putSLEB128 :: (Integral a, Bits a) => a -> [Word8]
putSLEB128 = (inline G.encodeSLEB128) pure

type ByteProvider a = State [Word8] a

{-# INLINE getByte #-}
getByte :: ByteProvider Word8
getByte = do
  wds <- get
  case wds of
    [] -> error "decode LEB128: Not enough bytes"
    (x:xs) -> put xs >> return x

{-# INLINEABLE getULEB128 #-}
-- | Decode a unsigned LEB128 encoded value from a list of bytes.
getULEB128 :: (Integral a, Bits a) => [Word8] -> (a,[Word8])
getULEB128 = runState ((inline G.decodeLEB128) getByte)

{-# INLINEABLE getSLEB128 #-}
-- | Decode a signed __S__LEB128 encoded value from a list of bytes.
getSLEB128 :: (Integral a, Bits a) => [Word8] -> (a,[Word8])
getSLEB128 = runState ((inline G.decodeSLEB128) getByte)