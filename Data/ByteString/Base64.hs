{-# LANGUAGE BangPatterns #-}

module Data.ByteString.Base64
    (
      encode
    , decodeLenient
    ) where

import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Data.ByteString (empty, pack, index)
import Data.ByteString.Internal (ByteString(..), mallocByteString)
import Data.ByteString.Unsafe (unsafeIndex)
import Data.Word (Word8, Word16, Word32)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrArray, withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, minusPtr, plusPtr)
import Foreign.Storable (peek, peekElemOff, poke, pokeElemOff)
import System.IO.Unsafe (unsafePerformIO)

peek8 :: Ptr Word8 -> IO Word8
peek8 = peek

poke8 :: Ptr Word8 -> Word8 -> IO ()
poke8 = poke

peek8_32 :: Ptr Word8 -> IO Word32
peek8_32 = fmap fromIntegral . peek8

encode :: ByteString -> ByteString
encode (PS sfp soff slen) = unsafePerformIO $ do
  let dlen = ((slen + 2) `div` 3) * 4
  dfp <- mallocByteString dlen
  withForeignPtr afp $ \aptr ->
    withForeignPtr encodeTable $ \ep ->
      withForeignPtr sfp $ \sptr -> do
        let aidx n = peek8 (aptr `plusPtr` n)
            sEnd = sptr `plusPtr` slen
            fill !dp !sp
              | sp `plusPtr` 2 >= sEnd = complete (castPtr dp) sp
              | otherwise = do
              i <- peek8_32 sp
              j <- peek8_32 (sp `plusPtr` 1)
              k <- peek8_32 (sp `plusPtr` 2)
              let w = (i `shiftL` 16) .|. (j `shiftL` 8) .|. k
                  enc = peekElemOff ep . fromIntegral
              poke dp =<< enc (w `shiftR` 12)
              poke (dp `plusPtr` 2) =<< enc (w .&. 0xfff)
              fill (dp `plusPtr` 4) (sp `plusPtr` 3)
            complete dp sp
                | sp == sEnd = return ()
                | otherwise  = do
              let peekSP n f = (f . fromIntegral) `fmap` peek8 (sp `plusPtr` n)
                  twoMore    = sp `plusPtr` 2 == sEnd
                  equals     = 0x3d :: Word8
              a <- peekSP 0 ((`shiftR` 2) . (.&. 0xfc))
              b <- peekSP 0 ((`shiftL` 4) . (.&. 0x03))
              !b' <- if twoMore
                     then peekSP 1 ((.|. b) . (`shiftR` 4) . (.&. 0xf0))
                     else return b
              poke8 dp =<< aidx a
              poke8 (dp `plusPtr` 1) =<< aidx b'
              c <- if twoMore
                   then peekSP 1 ((`shiftL` 2) . (.&. 0x0f))
                   else return equals
              poke8 (dp `plusPtr` 2) c
              poke8 (dp `plusPtr` 3) equals
        withForeignPtr dfp $ \dptr ->
          fill (castPtr dptr) (sptr `plusPtr` soff)
  return $! PS dfp 0 dlen

data D = D { dNext :: {-# UNPACK #-} !(Ptr Word8)
           , dValue :: {-# UNPACK #-} !Word32 }

decodeLenient :: ByteString -> ByteString
decodeLenient (PS sfp soff slen)
    | dlen <= 0 = empty
    | otherwise = unsafePerformIO $ do
  dfp <- mallocByteString dlen
  dbytes <- withForeignPtr decfp $ \decptr ->
    withForeignPtr sfp $ \sptr -> do
      let sEnd = sptr `plusPtr` slen
          fill !dp !sp !n
            | sp >= sEnd = return n
            | otherwise = do
            let look yieldPad = go
                  where
                    go p | p >= sEnd = return $! D (sEnd `plusPtr` (-1)) done
                         | otherwise = do
                      ix <- fromIntegral `fmap` peek8 p
                      v <- peek8 (decptr `plusPtr` ix)
                      if v /= 0 && (yieldPad || v /= done)
                        then return $! D (p `plusPtr` 1) (fromIntegral v)
                        else go (p `plusPtr` 1)
            !a <- look False sp
            !b <- look False (dNext a)
            !c <- look True  (dNext b)
            !d <- look True  (dNext c)
            let w = (dValue a `shiftL` 18) .|. (dValue b `shiftL` 12) .|.
                    (dValue c `shiftL` 6) .|. dValue d
            if dValue a == done || dValue b == done
              then return n
              else do
                poke8 dp $ fromIntegral (w `shiftR` 16)
                if dValue c == done
                  then return $! n + 1
                  else do
                    poke8 (dp `plusPtr` 1) $ fromIntegral (w `shiftR` 8)
                    if dValue d == done
                      then return $! n + 2
                      else do
                        poke8 (dp `plusPtr` 2) $ fromIntegral w
                        fill (dp `plusPtr` 3) (dNext d) (n+3)
      withForeignPtr dfp $ \dptr ->
        fill dptr (sptr `plusPtr` soff) 0
  return $! if dbytes > 0
            then PS dfp 0 dbytes
            else empty
  where dlen = ((slen + 3) `div` 4) * 3

alphabet :: ByteString
alphabet@(PS afp _ _) = pack $ [65..90] ++ [97..122] ++ [48..57] ++ [43,47]
{-# NOINLINE alphabet #-}

encodeTable :: ForeignPtr Word16
encodeTable = unsafePerformIO $ do
  fp <- mallocForeignPtrArray 4096
  let ix = fromIntegral . unsafeIndex alphabet
  withForeignPtr fp $ \p ->
    sequence_ [ pokeElemOff p (j*64+k) ((ix k `shiftL` 8) .|. ix j)
                | j <- [0..64], k <- [0..64] ]
  return fp
{-# NOINLINE encodeTable #-}

decodeTable :: ByteString
decodeTable@(PS decfp _ _) = pack $ replicate 43 0 ++ [ 62,0,0,0,63,52,53,54,55,56,57,58,59,60,
  61,0,0,0,done,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,
  22,23,24,25,0,0,0,0,0,0,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,
  44,45,46,47,48,49,50,51 ] ++ replicate 133 0
{-# NOINLINE decodeTable #-}

done :: Integral a => a
done = 99
