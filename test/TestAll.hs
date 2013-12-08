{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Arbitrary

import Data.Text (Text, pack)
import Data.Text.Identifiers

instance Arbitrary Text where
    arbitrary = fmap pack arbitrary

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testGroup "QuickCheck PowerMap.Data.IdSet"
            [ testProperty "hasId"          prop_hasId
            , testProperty "stableId"       prop_stableId
            , testProperty "keyRetrieval"   prop_keyRetrieval
            , testProperty "idempotent"     prop_idempotent
            ]
        ]

