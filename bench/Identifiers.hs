{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.DeepSeq
import Criterion.Main
import Data.Binary
import Data.Identifiers
import Data.Text (Text, pack)
    
genNames :: Int -> [Text]
genNames n = map (pack . show) . take n $ ([100000000..] :: [Int])

fromListI :: [Text] -> Identifiers Word32 Text
fromListI = fromList

main :: IO ()
main = do
    let setA = genNames 2000
        setB = genNames 200000
        idA = fromListI setA
        idB = fromListI setB
    defaultMain
        [ bgroup "fromList"
            [ setA `deepseq` bench "  2,000" $ nf fromListI setA
            , setB `deepseq` bench "200,000" $ nf fromListI setB
            ]
        , bgroup "lookupKey"
            [ idA `deepseq` bench "  2,000" $ nf (`lookupKey` 500)   idA
            , idB `deepseq` bench "200,000" $ nf (`lookupKey` 50000) idB
            ]
        , bgroup "lookupId"
            [ bench "  2,000" $ nf (`lookupId` "100000500") idA
            , bench "200,000" $ nf (`lookupId` "100050000") idB
            ]
        ]

