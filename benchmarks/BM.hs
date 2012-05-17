{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base64 as B
import qualified Data.ByteString.Base64.URL as U

sized name orig =
    bgroup name [
      bgroup "normal" [
        bench "decode" $ whnf B.decode enc
      , bench "decodeLenient" $ whnf B.decodeLenient enc
      , bench "encode" $ whnf B.encode orig
      ]
    , bgroup "url" [
        bench "decode" $ whnf U.decode enc
      , bench "decodeLenient" $ whnf U.decodeLenient enc
      , bench "encode" $ whnf U.encode orig
      ]
    ]
  where enc = U.encode orig

main :: IO ()
main = defaultMain [
         sized "small" input
       , sized "medium" (B.concat (replicate 100 input))
       , sized "large" (B.concat (replicate 10000 input))
       ]
  where input = "abcdABCD0123[];'"
