module Main where

import Criterion.Main
import Data.Text (pack)
import Data.Text.Identifiers

benchInsert :: Int -> Identifiers
benchInsert = fromList . genNames
    where genNames n = map (pack . show) . take n $ [1000000..]

main = defaultMain [
        bgroup "insert" [ bench "1000"   $ whnf benchInsert 1000
                        , bench "10000"  $ whnf benchInsert 10000
                        , bench "100000" $ whnf benchInsert 100000
                        ]
        ]

