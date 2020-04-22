{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
-- |
-- Module      : Data.ByteString.Base64.Internal
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast and efficient encoding and decoding of base64-encoded strings.

module Data.ByteString.Base64.Internal
    (
      encodeWith
    , decodeWithTable
    , decodeLenientWithTable
    , mkEncodeTable
    , joinWith
    , done
    , peek8, poke8, peek8_32
    , reChunkIn
    , Padding(..)
    ) where

import Data.Bits ((.|.), (.&.), shiftL, shiftR, unsafeShiftL, unsafeShiftR)
import qualified Data.ByteString as B
import Data.ByteString.Internal (ByteString(..), mallocByteString, memcpy,
                                 unsafeCreate)
import Data.Word (Word8, Word16, Word32)
import Control.Exception (assert)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, castForeignPtr)
import Foreign.Ptr (Ptr, castPtr, minusPtr, plusPtr)
import Foreign.Storable (peek, peekElemOff, poke, peekByteOff)
import System.IO.Unsafe (unsafePerformIO)

peek8 :: Ptr Word8 -> IO Word8
peek8 = peek

poke8 :: Ptr Word8 -> Word8 -> IO ()
poke8 = poke

peek8_32 :: Ptr Word8 -> IO Word32
peek8_32 = fmap fromIntegral . peek8


data Padding = Padded | Don'tCare | Unpadded deriving Eq

-- | Encode a string into base64 form.  The result will always be a multiple
-- of 4 bytes in length.
encodeWith :: Padding -> EncodeTable -> ByteString -> ByteString
encodeWith !padding (ET alfaFP encodeTable) (PS sfp soff slen)
    | slen > maxBound `div` 4 =
        error "Data.ByteString.Base64.encode: input too long"
    | otherwise = unsafePerformIO $ do
  let dlen = ((slen + 2) `div` 3) * 4
  dfp <- mallocByteString dlen
  withForeignPtr alfaFP $ \aptr ->
    withForeignPtr encodeTable $ \ep ->
      withForeignPtr sfp $ \sptr -> do
        let aidx n = peek8 (aptr `plusPtr` n)
            sEnd = sptr `plusPtr` (slen + soff)
            finish !n = return (PS dfp 0 n)
            fill !dp !sp !n
              | sp `plusPtr` 2 >= sEnd = complete (castPtr dp) sp n
              | otherwise = {-# SCC "encode/fill" #-} do
              i <- peek8_32 sp
              j <- peek8_32 (sp `plusPtr` 1)
              k <- peek8_32 (sp `plusPtr` 2)
              let w = (i `shiftL` 16) .|. (j `shiftL` 8) .|. k
                  enc = peekElemOff ep . fromIntegral
              poke dp =<< enc (w `shiftR` 12)
              poke (dp `plusPtr` 2) =<< enc (w .&. 0xfff)
              fill (dp `plusPtr` 4) (sp `plusPtr` 3) (n + 4)
            complete dp sp n
                | sp == sEnd = finish n
                | otherwise  = {-# SCC "encode/complete" #-} do
              let peekSP m f = (f . fromIntegral) `fmap` peek8 (sp `plusPtr` m)
                  twoMore    = sp `plusPtr` 2 == sEnd
                  equals     = 0x3d :: Word8
                  doPad = padding == Padded
                  {-# INLINE equals #-}
              !a <- peekSP 0 ((`shiftR` 2) . (.&. 0xfc))
              !b <- peekSP 0 ((`shiftL` 4) . (.&. 0x03))

              poke8 dp =<< aidx a

              if twoMore
                then do
                  !b' <- peekSP 1 ((.|. b) . (`shiftR` 4) . (.&. 0xf0))
                  !c <- aidx =<< peekSP 1 ((`shiftL` 2) . (.&. 0x0f))
                  poke8 (dp `plusPtr` 1) =<< aidx b'
                  poke8 (dp `plusPtr` 2) c

                  if doPad
                    then poke8 (dp `plusPtr` 3) equals >> finish (n + 4)
                    else finish (n + 3)
                else do
                  poke8 (dp `plusPtr` 1) =<< aidx b

                  if doPad
                    then do
                      poke8 (dp `plusPtr` 2) equals
                      poke8 (dp `plusPtr` 3) equals
                      finish (n + 4)
                    else finish (n + 2)


        withForeignPtr dfp $ \dptr ->
          fill (castPtr dptr) (sptr `plusPtr` soff) 0

data EncodeTable = ET !(ForeignPtr Word8) !(ForeignPtr Word16)

-- The encoding table is constructed such that the expansion of a 12-bit
-- block to a 16-bit block can be done by a single Word16 copy from the
-- correspoding table entry to the target address. The 16-bit blocks are
-- stored in big-endian order, as the indices into the table are built in
-- big-endian order.
mkEncodeTable :: ByteString -> EncodeTable
mkEncodeTable alphabet@(PS afp _ _) =
    case table of PS fp _ _ -> ET afp (castForeignPtr fp)
  where
    ix    = fromIntegral . B.index alphabet
    table = B.pack $ concat $ [ [ix j, ix k] | j <- [0..63], k <- [0..63] ]

-- | Efficiently intersperse a terminator string into another at
-- regular intervals, and terminate the input with it.
--
-- Examples:
--
-- > joinWith "|" 2 "----" = "--|--|"
--
-- > joinWith "\r\n" 3 "foobarbaz" = "foo\r\nbar\r\nbaz\r\n"
-- > joinWith "x" 3 "fo" = "fox"
joinWith :: ByteString  -- ^ String to intersperse and end with
         -> Int         -- ^ Interval at which to intersperse, in bytes
         -> ByteString  -- ^ String to transform
         -> ByteString
joinWith brk@(PS bfp boff blen) every' bs@(PS sfp soff slen)
    | every' <= 0 = error "invalid interval"
    | blen <= 0  = bs
    | B.null bs = brk
    | otherwise =
  unsafeCreate dlen $ \dptr ->
    withForeignPtr bfp $ \bptr -> do
      withForeignPtr sfp $ \sptr -> do
          let bp = bptr `plusPtr` boff
              sp0 = sptr `plusPtr` soff
              sEnd = sp0 `plusPtr` slen
              dLast = dptr `plusPtr` dlen
              loop !dp !sp !written
                  | dp == dLast = return ()
                  | otherwise = do
                let chunkSize = min every (sEnd `minusPtr` sp)
                memcpy dp sp (fromIntegral chunkSize)
                let dp' = dp `plusPtr` chunkSize
                memcpy dp' bp (fromIntegral blen)
                let written' = written + chunkSize + blen
                assert (written' <= dlen) $
                  loop (dp' `plusPtr` blen) (sp `plusPtr` chunkSize) written'
          loop dptr sp0 0
  where dlast = slen + blen * numBreaks
        every = min slen every'
        dlen | rmndr > 0   = dlast + blen
             | otherwise   = dlast
        (numBreaks, rmndr) = slen `divMod` every

-- | Decode a base64-encoded string.  This function strictly follows
-- the specification in
-- <http://tools.ietf.org/rfc/rfc4648 RFC 4648>.
-- This function takes the decoding table (for @base64@ or @base64url@) as
-- the first paramert.
decodeWithTable :: Padding -> ForeignPtr Word8 -> ByteString -> Either String ByteString
decodeWithTable _ _ (PS _ _ 0) = Right B.empty
decodeWithTable padding decodeFP bs@(PS !fp !o !l) = unsafePerformIO $
   case padding of
     Padded
       | r /= 0 -> err "Base64-encoded bytestring required to be padded"
       | otherwise -> go bs
     Don'tCare
       | r == 0 -> go bs
       | r == 2 -> go (B.append bs (B.replicate 2 0x3d))
       | r == 3 -> go (B.append bs (B.replicate 1 0x3d))
       | otherwise -> err "Base64-encoded bytestring has invalid size"
     Unpadded
       | r == 0 -> validateUnpadded (go bs)
       | r == 2 -> validateUnpadded (go (B.append bs (B.replicate 2 0x3d)))
       | r == 3 -> validateUnpadded (go (B.append bs (B.replicate 1 0x3d)))
       | otherwise -> err "Base64-encoded bytestring has invalid size"
  where
    err = return . Left

    (q, r) = (B.length bs) `divMod` 4

    dlen = q * 3

    validateUnpadded io = withForeignPtr fp $ \p -> do
      let !end = l + o
      a <- peek (plusPtr p (end - 1))
      b <- peek (plusPtr p (end - 2))

      let !pad = 0x3d :: Word8
      if a == pad || b == pad
      then err "Base64-encoded bytestring required to be unpadded"
      else io

    go (PS !sfp !soff !slen) = do
      dfp <- mallocByteString dlen
      withForeignPtr decodeFP $ \ !decptr ->
        withForeignPtr sfp $ \sptr ->
        withForeignPtr dfp $ \dptr ->
          decodeLoop decptr (plusPtr sptr soff) dptr (sptr `plusPtr` (slen + soff)) dfp

decodeLoop
    :: Ptr Word8
      -- ^ decoding table pointer
    -> Ptr Word8
      -- ^ source pointer
    -> Ptr Word8
      -- ^ destination pointer
    -> Ptr Word8
      -- ^ source end pointer
    -> ForeignPtr Word8
      -- ^ destination foreign pointer (used for finalizing string)
    -> IO (Either String ByteString)
decodeLoop !dtable !sptr !dptr !end !dfp = go dptr sptr
  where
    err p = return . Left
      $ "invalid character at offset: "
      ++ show (p `minusPtr` sptr)

    padErr p =  return . Left
      $ "invalid padding at offset: "
      ++ show (p `minusPtr` sptr)

    look :: Ptr Word8 -> IO Word32
    look !p = do
      !i <- peekByteOff p 0 :: IO Word8
      !v <- peekByteOff dtable (fromIntegral i) :: IO Word8
      return (fromIntegral v)

    go !dst !src
      | plusPtr src 4 >= end = do
        !a <- look src
        !b <- look (src `plusPtr` 1)
        !c <- look (src `plusPtr` 2)
        !d <- look (src `plusPtr` 3)
        finalChunk dst src a b c d

      | otherwise = do
        !a <- look src
        !b <- look (src `plusPtr` 1)
        !c <- look (src `plusPtr` 2)
        !d <- look (src `plusPtr` 3)
        decodeChunk dst src a b c d

    -- | Decodes chunks of 4 bytes at a time, recombining into
    -- 3 bytes. Note that in the inner loop stage, no padding
    -- characters are admissible.
    --
    decodeChunk !dst !src !a !b !c !d
     | a == 0x63 = padErr src
     | b == 0x63 = padErr (plusPtr src 1)
     | c == 0x63 = padErr (plusPtr src 2)
     | d == 0x63 = padErr (plusPtr src 3)
     | a == 0xff = err src
     | b == 0xff = err (plusPtr src 1)
     | c == 0xff = err (plusPtr src 2)
     | d == 0xff = err (plusPtr src 3)
     | otherwise = do
       let !w = ((unsafeShiftL a 18)
             .|. (unsafeShiftL b 12)
             .|. (unsafeShiftL c 6)
             .|. d) :: Word32

       poke8 dst (fromIntegral (unsafeShiftR w 16))
       poke8 (plusPtr dst 1) (fromIntegral (unsafeShiftR w 8))
       poke8 (plusPtr dst 2) (fromIntegral w)
       go (plusPtr dst 3) (plusPtr src 4)

    -- | Decode the final 4 bytes in the string, recombining into
    -- 3 bytes. Note that in this stage, we can have padding chars
    -- but only in the final 2 positions.
    --
    finalChunk !dst !src a b c d
      | a == 0x63 = padErr src
      | b == 0x63 = padErr (plusPtr src 1)
      | c == 0x63 && d /= 0x63 = err (plusPtr src 2) -- make sure padding is coherent.
      | a == 0xff = err src
      | b == 0xff = err (plusPtr src 1)
      | c == 0xff = err (plusPtr src 2)
      | d == 0xff = err (plusPtr src 3)
      | otherwise = do
        let !w = ((unsafeShiftL a 18)
              .|. (unsafeShiftL b 12)
              .|. (unsafeShiftL c 6)
              .|. d) :: Word32

        poke8 dst (fromIntegral (unsafeShiftR w 16))

        if c == 0x63 && d == 0x63
        then return $ Right $ PS dfp 0 (1 + (dst `minusPtr` dptr))
        else if d == 0x63
          then do
            poke8 (plusPtr dst 1) (fromIntegral (unsafeShiftR w 8))
            return $ Right $ PS dfp 0 (2 + (dst `minusPtr` dptr))
          else do
            poke8 (plusPtr dst 1) (fromIntegral (unsafeShiftR w 8))
            poke8 (plusPtr dst 2) (fromIntegral w)
            return $ Right $ PS dfp 0 (3 + (dst `minusPtr` dptr))


-- | Decode a base64-encoded string.  This function is lenient in
-- following the specification from
-- <http://tools.ietf.org/rfc/rfc4648 RFC 4648>, and will not
-- generate parse errors no matter how poor its input.  This function
-- takes the decoding table (for @base64@ or @base64url@) as the first
-- paramert.
decodeLenientWithTable :: ForeignPtr Word8 -> ByteString -> ByteString
decodeLenientWithTable decodeFP (PS sfp soff slen)
    | dlen <= 0 = B.empty
    | otherwise = unsafePerformIO $ do
  dfp <- mallocByteString dlen
  withForeignPtr decodeFP $ \ !decptr ->
    withForeignPtr sfp $ \ !sptr -> do
      let finish dbytes
              | dbytes > 0 = return (PS dfp 0 dbytes)
              | otherwise  = return B.empty
          sEnd = sptr `plusPtr` (slen + soff)
          fill !dp !sp !n
            | sp >= sEnd = finish n
            | otherwise = {-# SCC "decodeLenientWithTable/fill" #-}
            let look :: Bool -> Ptr Word8
                     -> (Ptr Word8 -> Word32 -> IO ByteString)
                     -> IO ByteString
                {-# INLINE look #-}
                look skipPad p0 f = go p0
                  where
                    go p | p >= sEnd = f (sEnd `plusPtr` (-1)) done
                         | otherwise = {-# SCC "decodeLenient/look" #-} do
                      ix <- fromIntegral `fmap` peek8 p
                      v <- peek8 (decptr `plusPtr` ix)
                      if v == x || (v == done && skipPad)
                        then go (p `plusPtr` 1)
                        else f (p `plusPtr` 1) (fromIntegral v)
            in look True sp $ \ !aNext !aValue ->
               look True aNext $ \ !bNext !bValue ->
                 if aValue == done || bValue == done
                 then finish n
                 else
                    look False bNext $ \ !cNext !cValue ->
                    look False cNext $ \ !dNext !dValue -> do
                      let w = (aValue `shiftL` 18) .|. (bValue `shiftL` 12) .|.
                              (cValue `shiftL` 6) .|. dValue
                      poke8 dp $ fromIntegral (w `shiftR` 16)
                      if cValue == done
                        then finish (n + 1)
                        else do
                          poke8 (dp `plusPtr` 1) $ fromIntegral (w `shiftR` 8)
                          if dValue == done
                            then finish (n + 2)
                            else do
                              poke8 (dp `plusPtr` 2) $ fromIntegral w
                              fill (dp `plusPtr` 3) dNext (n+3)
      withForeignPtr dfp $ \dptr ->
        fill dptr (sptr `plusPtr` soff) 0
  where dlen = ((slen + 3) `div` 4) * 3

x :: Integral a => a
x = 255
{-# INLINE x #-}

done :: Integral a => a
done = 99
{-# INLINE done #-}

-- This takes a list of ByteStrings, and returns a list in which each
-- (apart from possibly the last) has length that is a multiple of n
reChunkIn :: Int -> [ByteString] -> [ByteString]
reChunkIn !n = go
  where
    go [] = []
    go (y : ys) = case B.length y `divMod` n of
                    (_, 0) -> y : go ys
                    (d, _) -> case B.splitAt (d * n) y of
                                (prefix, suffix) -> prefix : fixup suffix ys
    fixup acc [] = [acc]
    fixup acc (z : zs) = case B.splitAt (n - B.length acc) z of
                           (prefix, suffix) ->
                             let acc' = acc `B.append` prefix
                             in if B.length acc' == n
                                then let zs' = if B.null suffix
                                               then          zs
                                               else suffix : zs
                                     in acc' : go zs'
                                else -- suffix must be null
                                    fixup acc' zs
