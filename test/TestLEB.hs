{-# LANGUAGE TemplateHaskell #-}

module Main
where

import Data.Monoid (mempty)
import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.QuickCheck.All

import Codec.LEB128.List
import Codec.LEB128.Generic

import Data.Int
import Data.Word


main = defaultMain tests

mainWithOpts = do
  -- Test options can also be specified in the code. The TestOptions
  -- type is an instance of the Monoid type class, so the easiest way
  -- to get an empty set of options is with `mempty`.
  let empty_test_opts = mempty :: TestOptions

  -- We update the empty TestOptions with our desired values.
  let my_test_opts = empty_test_opts {
    topt_maximum_generated_tests = Just 5000
  }

  -- Now we create an empty RunnerOptions in the same way, and add
  -- our TestOptions to it.
  let empty_runner_opts = mempty :: RunnerOptions
  let my_runner_opts = empty_runner_opts {
    ropt_test_options = Just my_test_opts
  }

  defaultMainWithOpts tests my_runner_opts

tests = [
        testGroup "Roundtrips" [
                testProperty "unsigned_w32" prop_roundTripUW32,
                testProperty "unsigned_i16" prop_roundTripUW16,
                testProperty "signed_i32" prop_roundTripSI32,
                testProperty "signed_i16" prop_roundTripSI16
            ]
        -- testGroup "Sorting Group 2" [
        --         testGroup "Nested Group 1" [
        --               testProperty "sort4" prop_sort4,
        --               testProperty "sort5" prop_sort5,
        --               testProperty "sort6" prop_sort6
        --             ],
        --         testProperty "sort7" prop_sort7,
        --         testCase "sort8" test_sort8,
        --         testCase "sort9" test_sort9
        --     ]
    ]

prop_roundTripUW32 :: Word32 -> Bool
prop_roundTripUW32 w =
  w == fst (getULEB128 (putULEB128 w))

prop_roundTripUW16 :: Word16 -> Bool
prop_roundTripUW16 w =
  w == fst ( getULEB128 (putULEB128 w))

prop_roundTripSI32 :: Int32 -> Bool
prop_roundTripSI32 w =
  w  == fst ( getSLEB128 (putSLEB128 w))

prop_roundTripSI16 :: Int16 -> Bool
prop_roundTripSI16 w =
  w  == fst (getSLEB128 (putSLEB128 w))

prop_roundTripBytes :: Bool
prop_roundTripBytes =
    let bytes = [201,202,203,1]
    in bytes == putSLEB128 (fst . getSLEB128 $ bytes :: Int)



-- --------------------------
-- return []
-- runTests :: IO Bool
-- runTests = $verboseCheckAll

-- main = runTests