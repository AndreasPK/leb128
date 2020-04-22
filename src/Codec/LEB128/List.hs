{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
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

import Codec.LEB128.Generic as G

-- | Encode a unsigned value in LEB128.
{-# INLINEABLE putULEB128 #-}
putULEB128 :: (Integral a, Bits a) => a -> [Word8]
putULEB128 = (inline G.encodeLEB128) pure

-- | Encode a signed value in __S__LEB128.
{-# INLINEABLE putSLEB128 #-}
putSLEB128 :: (Integral a, Bits a) => a -> [Word8]
putSLEB128 = (inline G.encodeSLEB128) pure

newtype ByteProvider a = BP { runBP :: [Word8] -> (a,[Word8]) }
    deriving (Functor)

instance Applicative ByteProvider where
  pure x = BP $ \bs -> (x,bs)
  BP wf <*> BP wa = BP $ \wds ->
    case wf wds of
        (f, wds') -> case wa wds' of
            (a,wds'') -> (f a, wds'')

instance Monad ByteProvider where
  return = pure
  (>>=) :: ByteProvider a -> (a -> ByteProvider b) -> ByteProvider b
  BP wa >>= f =
    BP $ \wds ->
      case wa wds of
        (a', wds') -> case f a'
          of BP r -> r wds'

{-# INLINE getByte #-}
getByte :: ByteProvider Word8
getByte = BP $ \wds -> split wds
  where
    split (x:xs) = (x,xs)
    split [] = error "ByteProvider: Not enough bytes"

-- | Decode a unsigned LEB128 encoded value from a list of bytes.
getULEB128 :: (Integral a, Bits a) => [Word8] -> (a,[Word8])
getULEB128 wds = (runBP ((inline G.decodeLEB128) getByte)) wds

-- | Decode a signed __S__LEB128 encoded value from a list of bytes.
getSLEB128 :: (Integral a, Bits a) => [Word8] -> (a,[Word8])
getSLEB128 wds = (runBP ((inline G.decodeSLEB128) getByte)) wds
