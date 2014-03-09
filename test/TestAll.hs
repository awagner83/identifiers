{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Data.Identifiers.Hashable as H
import qualified Data.Identifiers.ListLike as L

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testGroup "QuickCheck Data.Identifiers.Hashable"
            [ testProperty "hasId"                H.prop_hasId
            , testProperty "stableId"             H.prop_stableId
            , testProperty "keyRetrieval"         H.prop_keyRetrieval
            , testProperty "keyRetrievalUnsafe"   H.prop_keyRetrievalUnsafe
            , testProperty "idempotent"           H.prop_idempotent
            ]
        , testGroup "QuickCheck Data.Identifiers.ListLike"
            [ testProperty "hasId"                L.prop_hasId
            , testProperty "stableId"             L.prop_stableId
            , testProperty "keyRetrieval"         L.prop_keyRetrieval
            , testProperty "keyRetrievalUnsafe"   L.prop_keyRetrievalUnsafe
            , testProperty "idempotent"           L.prop_idempotent
            ]
        ]

