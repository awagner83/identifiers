{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Control.Applicative
import Control.DeepSeq
import Criterion.Main
import Data.Binary
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, pack, unpack)
import Data.Identifiers

instance Binary Text where
    put = put . unpack
    get = pack <$> get
    
genNames :: Int -> [Text]
genNames n = map (pack . show) . take n $ ([100000000..] :: [Int])

decodeI :: ByteString -> Identifiers Text
decodeI = decode

main :: IO ()
main = do
    let setA = genNames 1000
        setB = genNames 10000
        setC = genNames 100000
        idA = fromList setA
        idB = fromList setB
        idC = fromList setC
        encA = encode idA
        encB = encode idB
        encC = encode idC
    defaultMain
        [ bgroup "fromList"
            [ setA `deepseq` bench "  1,000" $ nf fromList setA
            , setB `deepseq` bench " 10,000" $ nf fromList setB
            , setC `deepseq` bench "100,000" $ nf fromList setC
            ]
        , bgroup "lookupKey"
            [ idA `deepseq` bench "  1,000" $ nf (flip lookupKey 500)   idA
            , idB `deepseq` bench " 10,000" $ nf (flip lookupKey 5000)  idB
            , idC `deepseq` bench "100,000" $ nf (flip lookupKey 50000) idC
            ]
        , bgroup "lookupId"
            [ bench "  1,000" $ nf (flip lookupId "100000500") idA
            , bench " 10,000" $ nf (flip lookupId "100005000") idB
            , bench "100,000" $ nf (flip lookupId "100050000") idC
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

