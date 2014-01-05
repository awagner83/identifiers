{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Control.DeepSeq
import Criterion.Main
import Data.Binary
import Data.ByteString.Lazy (ByteString)
import Data.Identifiers
    
genNames :: Int -> [String]
genNames n = map show . take n $ ([100000000..] :: [Int])

decodeI :: ByteString -> Identifiers Word32 String
decodeI = decode

fromListI :: [String] -> Identifiers Word32 String
fromListI = fromList

main :: IO ()
main = do
    let setA = genNames 1000
        setB = genNames 10000
        setC = genNames 100000
        idA = fromListI setA
        idB = fromListI setB
        idC = fromListI setC
        encA = encode idA
        encB = encode idB
        encC = encode idC
    defaultMain
        [ bgroup "fromList"
            [ setA `deepseq` bench "  1,000" $ nf fromListI setA
            , setB `deepseq` bench " 10,000" $ nf fromListI setB
            , setC `deepseq` bench "100,000" $ nf fromListI setC
            ]
        , bgroup "lookupKey"
            [ idA `deepseq` bench "  1,000" $ nf (`lookupKey` 500)   idA
            , idB `deepseq` bench " 10,000" $ nf (`lookupKey` 5000)  idB
            , idC `deepseq` bench "100,000" $ nf (`lookupKey` 50000) idC
            ]
        , bgroup "lookupId"
            [ bench "  1,000" $ nf (`lookupId` "100000500") idA
            , bench " 10,000" $ nf (`lookupId` "100005000") idB
            , bench "100,000" $ nf (`lookupId` "100050000") idC
            ]
        , bgroup "encode"
            [ bench "  1,000" $ nf encode idA
            , bench " 10,000" $ nf encode idB
            , bench "100,000" $ nf encode idC
            ]
        , bgroup "decode"
            [ encA `deepseq` bench "  1,000" $ nf decodeI encA
            , encB `deepseq` bench " 10,000" $ nf decodeI encB
            , encC `deepseq` bench "100,000" $ nf decodeI encC
            ]
        ]

