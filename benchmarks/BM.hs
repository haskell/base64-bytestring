{-# LANGUAGE OverloadedStrings #-}

import Control.DeepSeq (NFData(rnf))
import Criterion.Main
import qualified Data.ByteString.Base64 as B
import qualified Data.ByteString.Base64.Lazy as L
import qualified Data.ByteString.Base64.URL as U
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L

strict name orig =
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

instance NFData L.ByteString where
    rnf L.Empty        = ()
    rnf (L.Chunk _ ps) = rnf ps

lazy name orig =
    bgroup name [
      bench "decode" $ nf L.decode enc
    , bench "encode" $ nf L.encode orig
    ]
  where enc = L.encode orig

main :: IO ()
main = defaultMain [
         bgroup "lazy" [
           lazy "small" (L.fromChunks [input])
         , lazy "medium" (L.concat . replicate 16 . L.fromChunks . (:[]) .
                          B.concat $ replicate 8 input)
         , lazy "large" (L.concat . replicate 1280 . L.fromChunks . (:[]) .
                          B.concat $ replicate 8 input)
         ]
       , bgroup "strict" [
           strict "small" input
         , strict "medium" (B.concat (replicate 128 input))
         , strict "large" (B.concat (replicate 10240 input))
         ]
       ]
  where input = "abcdABCD0123[];'"
