{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Data.Primitive.ByteArray (
  ByteArray(..), MutableByteArray(..),

  newByteArray, newPinnedByteArray, newAlignedPinnedByteArray,
  readByteArray, writeByteArray, indexByteArray,
  unsafeFreezeByteArray,
  sizeofByteArray, sizeofMutableByteArray, sameMutableByteArray,
  byteArrayContents
) where

import Control.Monad.Primitive
import Data.Primitive.Types

import GHC.Base ( Int(..) )
import GHC.Prim

data ByteArray = ByteArray ByteArray#
data MutableByteArray m = MutableByteArray (MutableByteArray# (PrimState m))

newByteArray :: PrimMonad m => Int -> m (MutableByteArray m)
newByteArray (I# n#)
  = primitive (\s# -> case newByteArray# n# s# of
                        (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #))

newPinnedByteArray :: PrimMonad m => Int -> m (MutableByteArray m)
newPinnedByteArray (I# n#)
  = primitive (\s# -> case newPinnedByteArray# n# s# of
                        (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #))

newAlignedPinnedByteArray :: PrimMonad m => Int -> Int -> m (MutableByteArray m)
newAlignedPinnedByteArray (I# n#) (I# k#)
  = primitive (\s# -> case newAlignedPinnedByteArray# n# k# s# of
                        (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #))

byteArrayContents :: ByteArray -> Addr
byteArrayContents (ByteArray arr#) = Addr (byteArrayContents# arr#)

sameMutableByteArray :: MutableByteArray m -> MutableByteArray m -> Bool
sameMutableByteArray (MutableByteArray arr#) (MutableByteArray brr#)
  = sameMutableByteArray# arr# brr#

unsafeFreezeByteArray :: PrimMonad m => MutableByteArray m -> m ByteArray
unsafeFreezeByteArray (MutableByteArray arr#)
  = primitive (\s# -> case unsafeFreezeByteArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, ByteArray arr'# #))

sizeofByteArray :: ByteArray -> Int
sizeofByteArray (ByteArray arr#) = I# (sizeofByteArray# arr#)

sizeofMutableByteArray :: MutableByteArray s -> Int
sizeofMutableByteArray (MutableByteArray arr#) = I# (sizeofMutableByteArray# arr#)

indexByteArray :: Prim a => ByteArray -> Int -> a
indexByteArray (ByteArray arr#) (I# i#) = indexByteArray# arr# i#

readByteArray :: (Prim a, PrimMonad m)
              => MutableByteArray m -> Int -> m a
readByteArray (MutableByteArray arr#) (I# i#)
  = primitive (readByteArray# arr# i#)

writeByteArray :: (Prim a, PrimMonad m)
               => MutableByteArray m -> Int -> a -> m ()
writeByteArray (MutableByteArray arr#) (I# i#) x
  = primitive (\s# -> (# writeByteArray# arr# i# x s#, () #))

