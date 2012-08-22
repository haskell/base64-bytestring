{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Test.QuickCheck (Arbitrary(..), Positive(..))

import Control.Monad (liftM)
import qualified Data.ByteString.Base64          as Base64
import qualified Data.ByteString.Base64.Lazy     as LBase64
import qualified Data.ByteString.Base64.URL      as Base64URL
import qualified Data.ByteString.Base64.URL.Lazy as LBase64URL
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.String
import Test.HUnit hiding (Test)


main :: IO ()
main = defaultMain tests

data Impl bs = Impl String
                    (bs -> bs)
                    (bs -> Either String bs)
                    (bs -> bs)

tests :: [Test]
tests = [
    testGroup "joinWith" [
        testProperty "all_endsWith" joinWith_all_endsWith
      , testProperty "endsWith" joinWith_endsWith
    ]
  , testsRegular $ Impl "Base64"     Base64.encode     Base64.decode     Base64.decodeLenient
  , testsRegular $ Impl "LBase64"    LBase64.encode    LBase64.decode    LBase64.decodeLenient
  , testsURL     $ Impl "Base64URL"  Base64URL.encode  Base64URL.decode  Base64URL.decodeLenient
  , testsURL     $ Impl "LBase64URL" LBase64URL.encode LBase64URL.decode LBase64URL.decodeLenient
  ]

testsRegular :: (IsString bs, AllRepresentations bs, Show bs, Eq bs, Arbitrary bs) => Impl bs -> Test
testsRegular = testsWith base64_testData

testsURL :: (IsString bs, AllRepresentations bs, Show bs, Eq bs, Arbitrary bs) => Impl bs -> Test
testsURL = testsWith base64url_testData

testsWith :: (IsString bs, AllRepresentations bs, Show bs, Eq bs, Arbitrary bs)
          => [(bs, bs)] -> Impl bs -> Test
testsWith testData
          impl@(Impl name encode decode decodeLenient)
    = testGroup name [
        testProperty "decodeEncode" $
          genericDecodeEncode encode decode
      , testProperty "decodeEncode Lenient" $
          genericDecodeEncode encode
                              (liftM Right decodeLenient)
      , testGroup "base64-string tests" (string_tests testData impl)
    ]

instance Arbitrary ByteString where
  arbitrary = liftM B.pack arbitrary

-- Ideally the arbitrary instance would have arbitrary chunks as well as
-- arbitrary content
instance Arbitrary L.ByteString where
  arbitrary = liftM L.pack arbitrary

joinWith_endsWith :: ByteString -> Positive Int -> ByteString -> Bool
joinWith_endsWith brk (Positive int) str =
  brk `B.isSuffixOf` Base64.joinWith brk int str

chunksOf :: Int -> ByteString -> [ByteString]
chunksOf k s
  | B.null s  = []
  | otherwise = let (h,t) = B.splitAt k s
                in h : chunksOf k t

joinWith_all_endsWith :: ByteString -> Positive Int -> ByteString -> Bool
joinWith_all_endsWith brk (Positive int) str =
    all (brk `B.isSuffixOf`) . chunksOf k . Base64.joinWith brk int $ str
  where k = B.length brk + min int (B.length str)

-- | Decoding an encoded sintrg should produce the original string.
genericDecodeEncode :: (Arbitrary bs, Eq bs)
                    => (bs -> bs)
                    -> (bs -> Either String bs)
                    -> bs -> Bool
genericDecodeEncode enc dec x = case dec (enc x) of
                                  Left  _  -> False
                                  Right x' -> x == x'

--
-- Unit tests from base64-string
-- Copyright (c) Ian Lynagh, 2005, 2007.
--

string_tests :: forall bs
              . (IsString bs, AllRepresentations bs, Show bs, Eq bs)
             => [(bs, bs)] -> Impl bs -> [Test]
string_tests testData (Impl _ encode decode decodeLenient) =
  base64_string_test encode decode         testData ++
  base64_string_test encode decodeLenient' testData
  where decodeLenient' :: bs -> Either String bs
        decodeLenient' = liftM Right decodeLenient

base64_testData :: IsString bs => [(bs, bs)]
base64_testData = [("",                "")
                  ,("\0",              "AA==")
                  ,("\255",            "/w==")
                  ,("E",               "RQ==")
                  ,("Ex",              "RXg=")
                  ,("Exa",             "RXhh")
                  ,("Exam",            "RXhhbQ==")
                  ,("Examp",           "RXhhbXA=")
                  ,("Exampl",          "RXhhbXBs")
                  ,("Example",         "RXhhbXBsZQ==")
                  ,("Ex\0am\254ple",   "RXgAYW3+cGxl")
                  ,("Ex\0am\255ple",   "RXgAYW3/cGxl")
                  ]

base64url_testData :: IsString bs => [(bs, bs)]
base64url_testData = [("",                "")
                     ,("\0",              "AA==")
                     ,("\255",            "_w==")
                     ,("E",               "RQ==")
                     ,("Ex",              "RXg=")
                     ,("Exa",             "RXhh")
                     ,("Exam",            "RXhhbQ==")
                     ,("Examp",           "RXhhbXA=")
                     ,("Exampl",          "RXhhbXBs")
                     ,("Example",         "RXhhbXBsZQ==")
                     ,("Ex\0am\254ple",   "RXgAYW3-cGxl")
                     ,("Ex\0am\255ple",   "RXgAYW3_cGxl")
                     ]

-- | Generic test given encod enad decode funstions and a
-- list of (plain, encoded) pairs
base64_string_test :: (AllRepresentations bs, Eq bs, Show bs)
                   => (bs -> bs)
                   -> (bs -> Either String bs)
                   -> [(bs, bs)] -> [Test]
base64_string_test enc dec testData =
      [ testCase ("base64-string: Encode " ++ show plain)
                 (encoded_plain @?= rawEncoded)
      | (rawPlain, rawEncoded) <- testData,
        -- For lazy ByteStrings, we want to check not only ["foo"], but
        -- also ["f","oo"], ["f", "o", "o"] and ["fo", "o"]. The
        -- allRepresentations function gives us all representations of a
        -- lazy ByteString.
        plain   <- allRepresentations rawPlain,
        let encoded_plain = enc plain
      ] ++
      [ testCase ("base64-string: Decode " ++ show encoded)
                 (decoded_encoded @?= Right rawPlain)
      | (rawPlain, rawEncoded) <- testData,
        -- Again, we need to try all representations of lazy ByteStrings.
        encoded <- allRepresentations rawEncoded,
        let decoded_encoded = dec encoded
      ]

class AllRepresentations a where
    allRepresentations :: a -> [a]

instance AllRepresentations ByteString where
    allRepresentations bs = [bs]

instance AllRepresentations L.ByteString where
    -- TODO: Use L.toStrict instead of (B.concat . L.toChunks) once
    -- we can rely on a new enough bytestring
    allRepresentations = map L.fromChunks . allChunks . B.concat . L.toChunks
        where allChunks b
               | B.length b < 2 = [[b]]
               | otherwise
                  = concat [ map (prefix :) (allChunks suffix)
                           | let splits = zip (B.inits b) (B.tails b)
                             -- We don't want the first split (empty prefix)
                             -- The last split (empty suffix) gives us the
                             -- [b] case (toChunks ignores an "" element).
                           , (prefix, suffix) <- tail splits ]

