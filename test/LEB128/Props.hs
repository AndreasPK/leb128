{-# LANGUAGE TemplateHaskell #-}

module LEB128.Props

where

import Data.Monoid (mempty)
import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.QuickCheck.All

import Codec.LEB128

import Data.Int
import Data.Word
import Data.ByteString (ByteString)


prop_roundtrip_bs :: Large Word -> Bool
prop_roundtrip_bs (Large w) =
    w == (fst (fromULEB128ByteStringUnsafe $ toULEB128ByteString w))

prop_roundtrip_bs_2 :: Large Word -> Bool
prop_roundtrip_bs_2 (Large w) =
    toULEB128ByteString w == ((toULEB128ByteString :: Word -> ByteString) . fst . fromULEB128ByteStringUnsafe $ toULEB128ByteString w)

