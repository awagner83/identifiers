{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Data.Identifiers as G
import qualified Data.Identifiers.ListLike as L

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testGroup "QuickCheck Data.Identifiers"
            [ testProperty "hasId"                G.prop_hasId
            , testProperty "stableId"             G.prop_stableId
            , testProperty "keyRetrieval"         G.prop_keyRetrieval
            , testProperty "keyRetrievalUnsafe"   G.prop_keyRetrievalUnsafe
            , testProperty "idempotent"           G.prop_idempotent
            ]
        , testGroup "QuickCheck Data.Identifiers.ListLike"
            [ testProperty "hasId"                L.prop_hasId
            , testProperty "stableId"             L.prop_stableId
            , testProperty "keyRetrieval"         L.prop_keyRetrieval
            , testProperty "keyRetrievalUnsafe"   L.prop_keyRetrievalUnsafe
            , testProperty "idempotent"           L.prop_idempotent
            ]
        ]

