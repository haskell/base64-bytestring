{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main
import qualified Data.ByteString.Base64 as B
import qualified Data.ByteString.Base64.Lazy as L
import qualified Data.ByteString.Base64.URL as U
import qualified Data.ByteString.Base64.URL.Lazy as LU
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L

#if !MIN_VERSION_bytestring(0,10,0)
import Control.DeepSeq (NFData(rnf))
import qualified Data.ByteString.Lazy.Internal as L
#endif

strict :: String -> B.ByteString -> Benchmark
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
      , bench "encodeUnpadded" $ whnf U.encodeUnpadded orig
      ]
    ]
  where enc = U.encode orig

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData L.ByteString where
    rnf L.Empty        = ()
    rnf (L.Chunk _ ps) = rnf ps
#endif

lazy :: String -> L.ByteString -> Benchmark
lazy name orig =
    bgroup name [
      bgroup "normal" [
        bench "decode" $ nf L.decode enc
      , bench "decodeLenient" $ nf L.decodeLenient enc
      , bench "encode" $ nf L.encode orig
      ]
    , bgroup "url" [
        bench "decode" $ nf LU.decode enc
      , bench "decodeLenient" $ nf LU.decodeLenient enc
      , bench "encode" $ nf LU.encode orig
      , bench "encodeUnpadded" $ whnf LU.encodeUnpadded orig
      ]
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
