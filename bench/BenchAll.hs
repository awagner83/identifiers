module Main where

import Criterion.Main
import Data.Text (Text, pack)
import Data.Text.Identifiers

benchRoundtrip :: Int -> [Text]
benchRoundtrip = toList . fromList . genNames
    where genNames n = map (pack . show) . take n $ ([1000000..] :: [Int])

main :: IO ()
main = defaultMain [
        bgroup "insert" [ bench "1000"   $ nf benchRoundtrip 1000
                        , bench "10000"  $ nf benchRoundtrip 10000
                        , bench "100000" $ nf benchRoundtrip 100000
                        ]
        ]

