module Main where

import Criterion.Main
import Data.Text (pack)
import Data.Text.Identifiers

benchRoundtrip :: Int -> Identifiers
benchRoundtrip = fromList . genNames
    where genNames n = map (pack . show) . take n $ ([100000000..] :: [Int])

main :: IO ()
main = defaultMain [
        bgroup "insert" [ bench "    1,000" $ nf benchRoundtrip 1000
                        , bench "   10,000" $ nf benchRoundtrip 10000
                        , bench "  100,000" $ nf benchRoundtrip 100000
                        ]
        ]

