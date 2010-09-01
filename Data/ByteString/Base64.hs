{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Data.ByteString.Base64
    (
      encode
    , decode
    ) where

import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Data.ByteString.Char8 (ByteString, pack)
import Data.ByteString.Internal
import Data.ByteString.Unsafe
import Data.Word (Word8, Word16, Word32)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrArray, withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peek, peekElemOff, poke, pokeElemOff)
import System.IO.Unsafe (unsafePerformIO)

peek8 :: Ptr Word8 -> IO Word8
peek8 = peek

encode :: ByteString -> ByteString
encode (PS sary soff slen) = unsafePerformIO $ do
  let dlen = (((slen + 2) `div` 3) * 4)
  dary <- mallocByteString dlen
  withForeignPtr encTable $ \ep ->
    withForeignPtr sary $ \sptr ->
      withForeignPtr dary $ \dptr -> do
        let pp p = (fromIntegral `fmap` peek8 p) :: IO Word32
            sEnd = sptr `plusPtr` slen
            fill !dp !sp
              | sp `plusPtr` 2 >= sEnd = complete (castPtr dp) sp
              | otherwise = do
              i <- pp sp
              j <- pp (sp `plusPtr` 1)
              k <- pp (sp `plusPtr` 2)
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
              b' <- if twoMore
                    then peekSP 1 ((.|. b) . (`shiftR` 4) . (.&. 0xf0))
                    else return b
              poke dp (unsafeIndex alphabet a)
              poke (dp `plusPtr` 1) (unsafeIndex alphabet b')
              c <- if twoMore
                   then peekSP 1 ((`shiftL` 2) . (.&. 0x0f))
                   else return equals
              poke (dp `plusPtr` 2) c
              poke (dp `plusPtr` 3) equals
        fill (castPtr dptr) (sptr `plusPtr` soff)
  return $! PS dary 0 dlen

decode :: ByteString -> ByteString
decode = undefined

alphabet :: ByteString
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
{-# NOINLINE alphabet #-}

encTable :: ForeignPtr Word16
encTable = unsafePerformIO $ do
             fp <- mallocForeignPtrArray 4096
             let ix = fromIntegral . unsafeIndex alphabet
             withForeignPtr fp $ \p ->
               sequence_ [ pokeElemOff p (j*64+k) ((ix k `shiftL` 8) .|. ix j)
                           | j <- [0..64], k <- [0..64] ]
             return fp
{-# NOINLINE encTable #-}
