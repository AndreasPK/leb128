{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}

{- |
    Module      : Codec.LEB128.Generic
    Description : Encode values via (S)LEB128 to/from lists.
    Copyright   : (c) Andreas Klebinger 2020
    License     : BSD3
    Maintainer  : Andreas Klebinger
    Portability : GHC >= 7.10

   The implementation is backed by the generic algorithms defined
   in "Codec.LEB128.Generic".

   The code is quite fast but does not fuse.
-}

module Codec.LEB128.List
  ( fromULEB128
  , fromSLEB128
  , toULEB128
  , toSLEB128
  )
where

import Data.Bits
import Data.Word
import GHC.Magic

import Control.Monad.Trans.State.Strict

import Codec.LEB128.Constraints
import Codec.LEB128.Generic as G

-- | Encode a __unsigned__ value in LEB128.
{-# INLINEABLE toULEB128 #-}
toULEB128 :: LEB128 a => a -> [Word8]
toULEB128 = (inline G.encodeLEB128) pure

-- | Encode a __signed__ value in LEB128.
{-# INLINEABLE toSLEB128 #-}
toSLEB128 :: SLEB128 a => a -> [Word8]
toSLEB128 = (inline G.encodeSLEB128) pure

type ByteProvider a = State [Word8] a

{-# INLINE getByte #-}
getByte :: ByteProvider Word8
getByte = do
  wds <- get
  case wds of
    [] -> error "decode LEB128: Not enough bytes"
    (x:xs) -> put xs >> return x

{-# INLINEABLE fromULEB128 #-}
-- | Decode a __unsigned__ value from LEB128 encoding.
fromULEB128 :: LEB128 a => [Word8] -> (a,[Word8])
fromULEB128 = runState ((inline G.decodeLEB128) getByte)

{-# INLINEABLE fromSLEB128 #-}
-- | Decode a __signed__ value from SLEB128 encoding.
fromSLEB128 :: SLEB128 a => [Word8] -> (a,[Word8])
fromSLEB128 = runState ((inline G.decodeSLEB128) getByte)