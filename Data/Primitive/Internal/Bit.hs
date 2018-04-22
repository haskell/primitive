{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Primitive.Internal.Bit
 (
   MutableBitArray
 , newBitArray
 , readBitArray
 , setBitArray
 ) where

import Data.Primitive.ByteArray
import Control.Monad.Primitive
import Data.Bits

newtype MutableBitArray s = MBA (MutableByteArray s)

newBitArray :: PrimMonad m => Int -> m (MutableBitArray (PrimState m))
newBitArray n = do
  let s = ((n + wordSize - 1) `unsafeShiftR` 3)
  mary <- newByteArray s
  fillByteArray mary 0 s 0
  return (MBA mary)

readBitArray :: PrimMonad m => MutableBitArray (PrimState m) -> Int -> m Bool
readBitArray (MBA mry) i = do
  wd :: Word <- readByteArray mry (whichWord i)
  return $! (((wd `unsafeShiftR` whichBit i) .&. 1) == 1)

setBitArray :: PrimMonad m => MutableBitArray (PrimState m) -> Int -> m ()
setBitArray (MBA mry) i = do
  let ww = whichWord i
  wd :: Word <- readByteArray mry ww
  let wd' = wd .|. (1 `unsafeShiftL` (whichBit i))
  writeByteArray mry ww wd'

wordSize :: Int
wordSize = finiteBitSize (undefined :: Word)

ctlws :: Int
ctlws
  | wordSize == 64 = 6
  | wordSize == 32 = 5
  | otherwise = countTrailingZeros wordSize

whichWord :: Int -> Int
whichWord i = i `unsafeShiftR` ctlws

whichBit :: Int -> Int
whichBit i = i .&. (wordSize - 1)

{-
-- For debugging
freezeByteArray
  :: PrimMonad m => MutableByteArray (PrimState m) -> m ByteArray
freezeByteArray mary = do
  s <- getSizeofMutableByteArray mary
  cop <- newByteArray s
  copyMutableByteArray cop 0 mary 0 s
  unsafeFreezeByteArray cop

prant :: MutableBitArray RealWorld -> IO ()
prant (MBA x) = freezeByteArray x >>= print
-}
