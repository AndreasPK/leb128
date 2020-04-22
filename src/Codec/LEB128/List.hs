{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}

{- | Encode numbers as variable length byte lists
     using the LEB128 encoding.

     Specializations for regular Int/Word types are provided.
     Using other types is possible but might not perform as expected.

-}

module Codec.LEB128.List
where

import Data.Bits
import Data.Word
import Data.Int
import Data.List
import GHC.Magic

import Codec.LEB128.Generic as G

{-# INLINEABLE putULEB128 #-}
putULEB128 :: (Integral a, Bits a) => a -> [Word8]
putULEB128 = (inline G.encodeLEB128) pure

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

getByte :: ByteProvider Word8
getByte = BP $ \wds -> split wds
  where
    split (x:xs) = (x,xs)
    split [] = error "ByteProvider: Not enough bytes"

getULEB128 :: (Integral a, Bits a) => [Word8] -> (a,[Word8])
getULEB128 wds = (runBP ((inline G.decodeLEB128) getByte)) wds
