{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

{- | Encode numbers as variable length byte lists
     using the LEB128 encoding.

     Specializations for regular Int/Word types are provided.
     Using other types is possible but might not perform as expected.

-}

module Data.LEB.List where

import Data.Bits
import Data.Word
import Data.Int
import Data.List

{-# SPECIALISE putULEB128 :: Int8 -> [Word8] #-}
{-# SPECIALISE putULEB128 :: Int16 -> [Word8] #-}
{-# SPECIALISE putULEB128 :: Int32 -> [Word8] #-}
{-# SPECIALISE putULEB128 :: Int64 -> [Word8] #-}
{-# SPECIALISE putULEB128 :: Int -> [Word8] #-}
{-# SPECIALISE putULEB128 :: Word8 -> [Word8] #-}
{-# SPECIALISE putULEB128 :: Word16 -> [Word8] #-}
{-# SPECIALISE putULEB128 :: Word32 -> [Word8] #-}
{-# SPECIALISE putULEB128 :: Word64 -> [Word8] #-}
{-# SPECIALISE putULEB128 :: Word -> [Word8] #-}
{-# INLINEABLE putULEB128 #-}
-- | Encode a unsigned number to a list of bytes in unsigned LEB128 encoding.
putULEB128 :: forall a. (Integral a, Bits a) => a -> [Word8]
putULEB128 w
  | w < 0 = error "Can't encode negative numbers in unsigned LEB format."
  | w == 0 = [0]
  | w > 0 = unfoldr go w
  where
    go :: a -> Maybe (Word8, a)
    go w
      | w == 0 = Nothing
      | w <= (127 :: a)
      = let !x = fromIntegral w :: Word8
        in Just (x, 0)
      | otherwise =
        -- bit 7 (8th bit) indicates more to come.
        let !byte = setBit (fromIntegral w) 7 :: Word8
            !w' = (w `unsafeShiftR` 7)
        in Just (byte, w' )

{-# SPECIALISE getULEB128 :: [Word8] -> Int8 #-}
{-# SPECIALISE getULEB128 :: [Word8] -> Int16 #-}
{-# SPECIALISE getULEB128 :: [Word8] -> Int32 #-}
{-# SPECIALISE getULEB128 :: [Word8] -> Int64 #-}
{-# SPECIALISE getULEB128 :: [Word8] -> Int #-}
{-# SPECIALISE getULEB128 :: [Word8] -> Word8 #-}
{-# SPECIALISE getULEB128 :: [Word8] -> Word16 #-}
{-# SPECIALISE getULEB128 :: [Word8] -> Word32 #-}
{-# SPECIALISE getULEB128 :: [Word8] -> Word64 #-}
{-# SPECIALISE getULEB128 :: [Word8] -> Word #-}
{-# INLINEABLE getULEB128 #-}
-- | Consume a list of bytes and reconstruct a unsigned number.
getULEB128 :: forall a. (Integral a, Bits a) => [Word8] -> a
getULEB128 bytes =
    go 0 0 bytes
  where
    go :: Int -> a -> [Word8] -> a
    go !shift !w (!b:bytes) =
        let !hasMore = testBit b 7
            !val = w .|. ((clearBit (fromIntegral b) 7) `unsafeShiftL` shift) :: a
        in  if hasMore
                then let !shift' = shift + 7 in go shift' val bytes
                else val

{-# SPECIALISE putSLEB128 :: Int8 -> [Word8] #-}
{-# SPECIALISE putSLEB128 :: Int16 -> [Word8] #-}
{-# SPECIALISE putSLEB128 :: Int32 -> [Word8] #-}
{-# SPECIALISE putSLEB128 :: Int64 -> [Word8] #-}
{-# SPECIALISE putSLEB128 :: Int -> [Word8] #-}
{-# SPECIALISE putSLEB128 :: Word8 -> [Word8] #-}
{-# SPECIALISE putSLEB128 :: Word16 -> [Word8] #-}
{-# SPECIALISE putSLEB128 :: Word32 -> [Word8] #-}
{-# SPECIALISE putSLEB128 :: Word64 -> [Word8] #-}
{-# SPECIALISE putSLEB128 :: Word -> [Word8] #-}
{-# INLINEABLE putSLEB128 #-}
-- | Encode a signed number to a list of bytes in signed LEB128 encoding.
putSLEB128 :: forall a. (Integral a, Bits a) => a -> [Word8]
putSLEB128 initial = go initial
  where
    go :: a -> [Word8]
    go val = do
        let !byte = fromIntegral (clearBit val 7) :: Word8
        let !val' = val `unsafeShiftR` 7
        let !signBit = testBit byte 6
        let !done =
                -- Unsigned value, val' == 0 and and last value can
                -- be discriminated from a negative number.
                ((val' == 0 && not signBit) ||
                -- Signed value,
                 (val' == -1 && signBit))

        let !byte' = if done then byte else setBit byte 7
        -- putByte bh byte'

        if done then [byte'] else byte' : go val'

{-# SPECIALISE getSLEB128 :: [Word8] -> Int8 #-}
{-# SPECIALISE getSLEB128 :: [Word8] -> Int16 #-}
{-# SPECIALISE getSLEB128 :: [Word8] -> Int32 #-}
{-# SPECIALISE getSLEB128 :: [Word8] -> Int64 #-}
{-# SPECIALISE getSLEB128 :: [Word8] -> Int #-}
{-# SPECIALISE getSLEB128 :: [Word8] -> Word8 #-}
{-# SPECIALISE getSLEB128 :: [Word8] -> Word16 #-}
{-# SPECIALISE getSLEB128 :: [Word8] -> Word32 #-}
{-# SPECIALISE getSLEB128 :: [Word8] -> Word64 #-}
{-# SPECIALISE getSLEB128 :: [Word8] -> Word #-}
{-# INLINEABLE getSLEB128 #-}
-- | Decode a signed number from a list of bytes in signed LEB128 encoding.
getSLEB128 :: forall a. (Integral a, FiniteBits a) => [Word8] -> a
getSLEB128 bytes =
    if signed
        then ((-1 `unsafeShiftL` shift) .|. val)
        else val
    where
        (val,shift,signed) = go 0 0 bytes
        go :: Int -> a -> [Word8] -> (a,Int,Bool)
        go shift val (byte:bytes) =
            -- byte <- getByte bh
            let !byteVal = fromIntegral (clearBit byte 7) :: a
                !val' = val .|. (byteVal `unsafeShiftL` shift)
                !more = testBit byte 7
                !shift' = shift+7
            in  if more
                    then go (shift') val' bytes
                    else
                        let !signed = testBit byte 6
                        in (val',shift',signed)
