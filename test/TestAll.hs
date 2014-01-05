{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Identifiers

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testGroup "QuickCheck PowerMap.Data.IdSet"
            [ testProperty "hasId"                prop_hasId
            , testProperty "stableId"             prop_stableId
            , testProperty "keyRetrieval"         prop_keyRetrieval
            , testProperty "keyRetrievalUnsafe"   prop_keyRetrievalUnsafe
            , testProperty "idempotent"           prop_idempotent
            ]
        ]

