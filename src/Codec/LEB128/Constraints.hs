{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
    Module      : Codec.LEB128.Constraints
    Description : Constrain encoding to compatible types.
    Copyright   : (c) Andreas Klebinger 2020
    License     : BSD3
    Maintainer  : Andreas Klebinger
    Portability : GHC >= 7.10

    SLEB128 is a synonym for the required constraints to encode values
    in the signed version of LEB128.

    LEB128 is a typeclass which by default limits encoding in unsigned LEB128
    format to GHC-Provided untyped valued.

    UnsafeAnyLEB128 is a newtype providing an explicit way to avoid this
    restriction for cases where the type is signed, but values are not.
-}

module Codec.LEB128.Constraints
    ( LEB128
    , SLEB128
    , UnsafeAnyLEB128(..)
    )
where

import Numeric.Natural
import Data.Word
import Data.Int
import Data.Bits

newtype UnsafeAnyLEB128 a = UnsafeAnyLEB128 a deriving (Eq,Bits,Num,Ord,Real,Enum,Integral)

-- | Indicates that a type can safely be encoded as (unsigned) LEB128.
class (Bits a, Integral a) => LEB128 a where
-- | Indicates that a type can safely be encoded as (signed) SLEB128.
class (Bits a, Integral a) => SLEB128 a where

-- | Unsafe escape hatch to force a particular encoding.
instance (Bits a, Integral a) => LEB128  (UnsafeAnyLEB128 a)
-- | Unsafe escape hatch to force a particular encoding.
instance (Bits a, Integral a) => SLEB128 (UnsafeAnyLEB128 a)

-- Some instances
instance LEB128 Natural
instance LEB128 Word
instance LEB128 Word8
instance LEB128 Word16
instance LEB128 Word32
instance LEB128 Word64

-- Some instances
instance SLEB128 Integer
instance SLEB128 Int
instance SLEB128 Int8
instance SLEB128 Int16
instance SLEB128 Int32
instance SLEB128 Int64

