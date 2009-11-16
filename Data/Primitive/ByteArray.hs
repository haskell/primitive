{-# LANGUAGE MagicHash, UnboxedTuples #-}

-- |
-- Module      : Data.Primitive.ByteArray
-- Copyright   : (c) Roman Leshchinskiy 2009
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
-- 
-- Primitive operations on ByteArrays
--

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

-- | Byte arrays
data ByteArray = ByteArray ByteArray#

-- | Mutable byte arrays
data MutableByteArray m = MutableByteArray (MutableByteArray# (PrimState m))

-- | Create a new mutable byte array of the specified size.
newByteArray :: PrimMonad m => Int -> m (MutableByteArray m)
{-# INLINE newByteArray #-}
newByteArray (I# n#)
  = primitive (\s# -> case newByteArray# n# s# of
                        (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #))

-- | Create a /pinned/ byte array of the specified size. The garbage collector
-- is guaranteed not to move it.
newPinnedByteArray :: PrimMonad m => Int -> m (MutableByteArray m)
{-# INLINE newPinnedByteArray #-}
newPinnedByteArray (I# n#)
  = primitive (\s# -> case newPinnedByteArray# n# s# of
                        (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #))

-- | Create a /pinned/ byte array of the specified size and with the give
-- alignment. The garbage collector is guaranteed not to move it.
newAlignedPinnedByteArray :: PrimMonad m => Int -> Int -> m (MutableByteArray m)
{-# INLINE newAlignedPinnedByteArray #-}
newAlignedPinnedByteArray (I# n#) (I# k#)
  = primitive (\s# -> case newAlignedPinnedByteArray# n# k# s# of
                        (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #))

-- | Yield a pointer to the array's data. This operation is only safe on
-- /pinned/ byte arrays allocated by 'newPinnedByteArray' or
-- 'newAlignedPinnedByteArray'.
byteArrayContents :: ByteArray -> Addr
{-# INLINE byteArrayContents #-}
byteArrayContents (ByteArray arr#) = Addr (byteArrayContents# arr#)

-- | Check if the two arrays refer to the same memory block.
sameMutableByteArray :: MutableByteArray m -> MutableByteArray m -> Bool
{-# INLINE sameMutableByteArray #-}
sameMutableByteArray (MutableByteArray arr#) (MutableByteArray brr#)
  = sameMutableByteArray# arr# brr#

-- | Convert a mutable byte array to an immutable one without copying. The
-- array should not be modified after the conversion.
unsafeFreezeByteArray :: PrimMonad m => MutableByteArray m -> m ByteArray
{-# INLINE unsafeFreezeByteArray #-}
unsafeFreezeByteArray (MutableByteArray arr#)
  = primitive (\s# -> case unsafeFreezeByteArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, ByteArray arr'# #))

-- | Size of the byte array.
sizeofByteArray :: ByteArray -> Int
{-# INLINE sizeofByteArray #-}
sizeofByteArray (ByteArray arr#) = I# (sizeofByteArray# arr#)

-- | Size of the mutable byte array.
sizeofMutableByteArray :: MutableByteArray s -> Int
{-# INLINE sizeofMutableByteArray #-}
sizeofMutableByteArray (MutableByteArray arr#) = I# (sizeofMutableByteArray# arr#)

-- | Read a primitive value from the byte array. The offset is given in
-- elements of type @a@ rather than in bytes.
indexByteArray :: Prim a => ByteArray -> Int -> a
{-# INLINE indexByteArray #-}
indexByteArray (ByteArray arr#) (I# i#) = indexByteArray# arr# i#

-- | Read a primitive value from the byte array. The offset is given in
-- elements of type @a@ rather than in bytes.
readByteArray :: (Prim a, PrimMonad m) => MutableByteArray m -> Int -> m a
{-# INLINE readByteArray #-}
readByteArray (MutableByteArray arr#) (I# i#)
  = primitive (readByteArray# arr# i#)

-- | Write a primitive value to the byte array. The offset is given in
-- elements of type @a@ rather than in bytes.
writeByteArray :: (Prim a, PrimMonad m)
               => MutableByteArray m -> Int -> a -> m ()
{-# INLINE writeByteArray #-}
writeByteArray (MutableByteArray arr#) (I# i#) x
  = primitive_ (writeByteArray# arr# i# x)

