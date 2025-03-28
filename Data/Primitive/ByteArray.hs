{-# LANGUAGE BangPatterns, CPP, MagicHash, UnboxedTuples, UnliftedFFITypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Data.Primitive.ByteArray
-- Copyright   : (c) Roman Leshchinskiy 2009-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Primitive operations on byte arrays. Most functions in this module include
-- an element type in their type signature and interpret the unit for offsets
-- and lengths as that element. A few functions (e.g. 'copyByteArray',
-- 'freezeByteArray') do not include an element type. Such functions
-- interpret offsets and lengths as units of 8-bit words.

module Data.Primitive.ByteArray (
  -- * Types
  ByteArray(..), MutableByteArray(..), ByteArray#, MutableByteArray#,

  -- * Allocation
  newByteArray, newPinnedByteArray, newAlignedPinnedByteArray,
  resizeMutableByteArray,
  shrinkMutableByteArray,

  -- * Element access
  readByteArray, writeByteArray, indexByteArray,
  -- * Char Element Access
  -- $charElementAccess
  readCharArray, writeCharArray, indexCharArray,

  -- * Constructing
  emptyByteArray,
  byteArrayFromList, byteArrayFromListN,

  -- * Folding
  foldrByteArray,

  -- * Comparing
  compareByteArrays,

  -- * Freezing and thawing
  freezeByteArray, thawByteArray, runByteArray, createByteArray,
  unsafeFreezeByteArray, unsafeThawByteArray,

  -- * Block operations
  copyByteArray, copyMutableByteArray,
  copyByteArrayToPtr, copyMutableByteArrayToPtr,
  copyByteArrayToAddr, copyMutableByteArrayToAddr,
  copyPtrToMutableByteArray,
  moveByteArray,
  setByteArray, fillByteArray,
  cloneByteArray, cloneMutableByteArray,

  -- * Information
  sizeofByteArray,
  sizeofMutableByteArray, getSizeofMutableByteArray, sameMutableByteArray,
#if __GLASGOW_HASKELL__ >= 802
  isByteArrayPinned, isMutableByteArrayPinned,
#endif
  byteArrayAsForeignPtr,
  mutableByteArrayAsForeignPtr,
  byteArrayContents,
  withByteArrayContents,
  mutableByteArrayContents,
  withMutableByteArrayContents

) where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Primitive.Types
import Data.Proxy

import qualified GHC.ST as GHCST

import Data.Word ( Word8 )
#if __GLASGOW_HASKELL__ >= 802
import qualified GHC.Exts as Exts
#endif
import GHC.Exts hiding (setByteArray#)
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents(..))

#if __GLASGOW_HASKELL__ < 804
import Foreign.C.Types
import System.IO.Unsafe (unsafeDupablePerformIO)
#endif

import Data.Array.Byte (ByteArray(..), MutableByteArray(..))

import Data.Primitive.Internal.Operations (mutableByteArrayContentsShim)

-- | Create a new mutable byte array of the specified size in bytes.
-- The underlying memory is left uninitialized.
--
-- /Note:/ this function does not check if the input is non-negative.
newByteArray :: PrimMonad m => Int -> m (MutableByteArray (PrimState m))
{-# INLINE newByteArray #-}
newByteArray (I# n#)
  = primitive (\s# -> case newByteArray# n# s# of
                        (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #))

-- | Create a /pinned/ byte array of the specified size in bytes. The garbage
-- collector is guaranteed not to move it. The underlying memory is left uninitialized.
--
-- /Note:/ this function does not check if the input is non-negative.
newPinnedByteArray :: PrimMonad m => Int -> m (MutableByteArray (PrimState m))
{-# INLINE newPinnedByteArray #-}
newPinnedByteArray (I# n#)
  = primitive (\s# -> case newPinnedByteArray# n# s# of
                        (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #))

-- | Create a /pinned/ byte array of the specified size in bytes and with the
-- given alignment. The garbage collector is guaranteed not to move it.
-- The underlying memory is left uninitialized.
--
-- /Note:/ this function does not check if the input is non-negative.
newAlignedPinnedByteArray
  :: PrimMonad m
  => Int  -- ^ size
  -> Int  -- ^ alignment
  -> m (MutableByteArray (PrimState m))
{-# INLINE newAlignedPinnedByteArray #-}
newAlignedPinnedByteArray (I# n#) (I# k#)
  = primitive (\s# -> case newAlignedPinnedByteArray# n# k# s# of
                        (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #))

-- | Create a foreign pointer that points to the array's data. This operation
-- is only safe on /pinned/ byte arrays.  The array's data is not garbage
-- collected while references to the foreign pointer exist. Writing to the
-- array through the foreign pointer results in undefined behavior.
byteArrayAsForeignPtr :: ByteArray -> ForeignPtr Word8
{-# INLINE byteArrayAsForeignPtr #-}
byteArrayAsForeignPtr (ByteArray arr#) = ForeignPtr (byteArrayContents# arr#) (PlainPtr (unsafeCoerce# arr#))


-- | Variant of 'byteArrayAsForeignPtr' for mutable byte arrays. Similarly, this
-- is only safe on /pinned/ mutable byte arrays. This function differs from the
-- variant for immutable arrays in that it is safe to write to the array though
-- the foreign pointer.
mutableByteArrayAsForeignPtr :: MutableByteArray RealWorld -> ForeignPtr Word8
{-# INLINE mutableByteArrayAsForeignPtr #-}
mutableByteArrayAsForeignPtr (MutableByteArray arr#) = ForeignPtr (mutableByteArrayContentsShim arr#) (PlainPtr arr#)

-- | Yield a pointer to the array's data. This operation is only safe on
-- /pinned/ byte arrays. Byte arrays allocated by 'newPinnedByteArray' and
-- 'newAlignedPinnedByteArray' are guaranteed to be pinned. Byte arrays
-- allocated by 'newByteArray' may or may not be pinned (use
-- 'isByteArrayPinned' to figure out).
--
-- Prefer 'withByteArrayContents', which ensures that the array is not
-- garbage collected while the pointer is being used.
byteArrayContents :: ByteArray -> Ptr Word8
{-# INLINE byteArrayContents #-}
byteArrayContents (ByteArray arr#) = Ptr (byteArrayContents# arr#)

-- | A composition of 'byteArrayContents' and 'keepAliveUnlifted'.
-- The callback function must not return the pointer. The argument byte
-- array must be /pinned/. See 'byteArrayContents' for an explanation
-- of which byte arrays are pinned.
--
-- Note: This could be implemented with 'keepAlive' instead of
-- 'keepAliveUnlifted', but 'keepAlive' here would cause GHC to materialize
-- the wrapper data constructor on the heap.
withByteArrayContents :: PrimBase m => ByteArray -> (Ptr Word8 -> m a) -> m a
{-# INLINE withByteArrayContents #-}
withByteArrayContents (ByteArray arr#) f =
  keepAliveUnlifted arr# (f (Ptr (byteArrayContents# arr#)))

-- | Yield a pointer to the array's data. This operation is only safe on
-- /pinned/ byte arrays. See 'byteArrayContents' for an explanation
-- of which byte arrays are pinned.
--
-- Prefer 'withByteArrayContents', which ensures that the array is not
-- garbage collected while the pointer is being used.
mutableByteArrayContents :: MutableByteArray s -> Ptr Word8
{-# INLINE mutableByteArrayContents #-}
mutableByteArrayContents (MutableByteArray arr#) = Ptr (mutableByteArrayContentsShim arr#)

-- | A composition of 'mutableByteArrayContents' and 'keepAliveUnlifted'.
-- The callback function must not return the pointer. The argument byte
-- array must be /pinned/. See 'byteArrayContents' for an explanation
-- of which byte arrays are pinned.
withMutableByteArrayContents :: PrimBase m => MutableByteArray (PrimState m) -> (Ptr Word8 -> m a) -> m a
{-# INLINE withMutableByteArrayContents #-}
withMutableByteArrayContents (MutableByteArray arr#) f =
  keepAliveUnlifted arr# (f (Ptr (mutableByteArrayContentsShim arr#)))

-- | Check if the two arrays refer to the same memory block.
sameMutableByteArray :: MutableByteArray s -> MutableByteArray s -> Bool
{-# INLINE sameMutableByteArray #-}
sameMutableByteArray (MutableByteArray arr#) (MutableByteArray brr#)
  = isTrue# (sameMutableByteArray# arr# brr#)

-- | Resize a mutable byte array. The new size is given in bytes.
--
-- This will either resize the array in-place or, if not possible, allocate the
-- contents into a new, unpinned array and copy the original array's contents.
--
-- To avoid undefined behaviour, the original 'MutableByteArray' shall not be
-- accessed anymore after a 'resizeMutableByteArray' has been performed.
-- Moreover, no reference to the old one should be kept in order to allow
-- garbage collection of the original 'MutableByteArray' in case a new
-- 'MutableByteArray' had to be allocated.
--
-- @since 0.6.4.0
resizeMutableByteArray
  :: PrimMonad m => MutableByteArray (PrimState m) -> Int
                 -> m (MutableByteArray (PrimState m))
{-# INLINE resizeMutableByteArray #-}
resizeMutableByteArray (MutableByteArray arr#) (I# n#)
  = primitive (\s# -> case resizeMutableByteArray# arr# n# s# of
                        (# s'#, arr'# #) -> (# s'#, MutableByteArray arr'# #))

-- | Get the size of a byte array in bytes. Unlike 'sizeofMutableByteArray',
-- this function ensures sequencing in the presence of resizing.
getSizeofMutableByteArray
  :: PrimMonad m => MutableByteArray (PrimState m) -> m Int
{-# INLINE getSizeofMutableByteArray #-}
#if __GLASGOW_HASKELL__ >= 801
getSizeofMutableByteArray (MutableByteArray arr#)
  = primitive (\s# -> case getSizeofMutableByteArray# arr# s# of
                        (# s'#, n# #) -> (# s'#, I# n# #))
#else
getSizeofMutableByteArray arr
  = return (sizeofMutableByteArray arr)
#endif

-- | Create an immutable copy of a slice of a byte array. The offset and
-- length are given in bytes.
--
-- This operation makes a copy of the specified section, so it is safe to
-- continue using the mutable array afterward.
--
-- /Note:/ The provided array should contain the full subrange
-- specified by the two Ints, but this is not checked.
freezeByteArray
  :: PrimMonad m
  => MutableByteArray (PrimState m) -- ^ source
  -> Int                            -- ^ offset in bytes
  -> Int                            -- ^ length in bytes
  -> m ByteArray
{-# INLINE freezeByteArray #-}
freezeByteArray !src !off !len = do
  dst <- newByteArray len
  copyMutableByteArray dst 0 src off len
  unsafeFreezeByteArray dst

-- | Create a mutable byte array from a slice of an immutable byte array.
-- The offset and length are given in bytes.
--
-- This operation makes a copy of the specified slice, so it is safe to
-- use the immutable array afterward.
--
-- /Note:/ The provided array should contain the full subrange
-- specified by the two Ints, but this is not checked.
--
-- @since 0.7.2.0
thawByteArray
  :: PrimMonad m
  => ByteArray -- ^ source
  -> Int       -- ^ offset in bytes
  -> Int       -- ^ length in bytes
  -> m (MutableByteArray (PrimState m))
{-# INLINE thawByteArray #-}
thawByteArray !src !off !len = do
  dst <- newByteArray len
  copyByteArray dst 0 src off len
  return dst

-- | Convert a mutable byte array to an immutable one without copying. The
-- array should not be modified after the conversion.
unsafeFreezeByteArray
  :: PrimMonad m => MutableByteArray (PrimState m) -> m ByteArray
{-# INLINE unsafeFreezeByteArray #-}
unsafeFreezeByteArray (MutableByteArray arr#)
  = primitive (\s# -> case unsafeFreezeByteArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, ByteArray arr'# #))

-- | Convert an immutable byte array to a mutable one without copying. The
-- original array should not be used after the conversion.
unsafeThawByteArray
  :: PrimMonad m => ByteArray -> m (MutableByteArray (PrimState m))
{-# INLINE unsafeThawByteArray #-}
unsafeThawByteArray (ByteArray arr#)
  = primitive (\s# -> (# s#, MutableByteArray (unsafeCoerce# arr#) #))

-- | Size of the byte array in bytes.
sizeofByteArray :: ByteArray -> Int
{-# INLINE sizeofByteArray #-}
sizeofByteArray (ByteArray arr#) = I# (sizeofByteArray# arr#)

-- | Size of the mutable byte array in bytes.
--
-- This function is deprecated and will be removed. Its behavior
-- is undefined if 'resizeMutableByteArray' is ever called on the mutable
-- byte array given as the argument. Prefer 'getSizeofMutableByteArray',
-- which ensures correct sequencing in the presence of resizing.
sizeofMutableByteArray :: MutableByteArray s -> Int
{-# INLINE sizeofMutableByteArray #-}
{-# DEPRECATED sizeofMutableByteArray "use getSizeofMutableByteArray instead" #-}
sizeofMutableByteArray (MutableByteArray arr#) = I# (sizeofMutableByteArray# arr#)

-- | Shrink a mutable byte array. The new size is given in bytes.
-- It must be smaller than the old size. The array will be resized in place.
--
-- @since 0.7.1.0
shrinkMutableByteArray :: PrimMonad m
  => MutableByteArray (PrimState m)
  -> Int -- ^ new size
  -> m ()
{-# INLINE shrinkMutableByteArray #-}
shrinkMutableByteArray (MutableByteArray arr#) (I# n#)
  = primitive_ (shrinkMutableByteArray# arr# n#)

#if __GLASGOW_HASKELL__ >= 802
-- | Check whether or not the byte array is pinned. Pinned byte arrays cannot
-- be moved by the garbage collector. It is safe to use 'byteArrayContents' on
-- such byte arrays.
--
-- Caution: This function is only available when compiling with GHC 8.2 or
-- newer.
--
-- @since 0.6.4.0
isByteArrayPinned :: ByteArray -> Bool
{-# INLINE isByteArrayPinned #-}
isByteArrayPinned (ByteArray arr#) = isTrue# (Exts.isByteArrayPinned# arr#)

-- | Check whether or not the mutable byte array is pinned.
--
-- Caution: This function is only available when compiling with GHC 8.2 or
-- newer.
--
-- @since 0.6.4.0
isMutableByteArrayPinned :: MutableByteArray s -> Bool
{-# INLINE isMutableByteArrayPinned #-}
isMutableByteArrayPinned (MutableByteArray marr#) = isTrue# (Exts.isMutableByteArrayPinned# marr#)
#endif

-- | Read a primitive value from the byte array. The offset is given in
-- elements of type @a@ rather than in bytes.
--
-- /Note:/ this function does not do bounds checking.
indexByteArray :: Prim a => ByteArray -> Int -> a
{-# INLINE indexByteArray #-}
indexByteArray (ByteArray arr#) (I# i#) = indexByteArray# arr# i#

-- | Read a primitive value from the byte array. The offset is given in
-- elements of type @a@ rather than in bytes.
--
-- /Note:/ this function does not do bounds checking.
readByteArray
  :: (Prim a, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> m a
{-# INLINE readByteArray #-}
readByteArray (MutableByteArray arr#) (I# i#)
  = primitive (readByteArray# arr# i#)

-- | Write a primitive value to the byte array. The offset is given in
-- elements of type @a@ rather than in bytes.
--
-- /Note:/ this function does not do bounds checking.
writeByteArray
  :: (Prim a, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> a -> m ()
{-# INLINE writeByteArray #-}
writeByteArray (MutableByteArray arr#) (I# i#) x
  = primitive_ (writeByteArray# arr# i# x)

-- | Right-fold over the elements of a 'ByteArray'.
foldrByteArray :: forall a b. (Prim a) => (a -> b -> b) -> b -> ByteArray -> b
{-# INLINE foldrByteArray #-}
foldrByteArray f z arr = go 0
  where
    go i
      | i < maxI  = f (indexByteArray arr i) (go (i + 1))
      | otherwise = z
    maxI = sizeofByteArray arr `quot` sizeOfType @a

-- | Create a 'ByteArray' from a list.
--
-- @byteArrayFromList xs = `byteArrayFromListN` (length xs) xs@
byteArrayFromList :: Prim a => [a] -> ByteArray
byteArrayFromList xs = byteArrayFromListN (length xs) xs

-- | Create a 'ByteArray' from a list of a known length. If the length
-- of the list does not match the given length, this throws an exception.

-- See Note [fromListN] in Data.Primitive.Array
byteArrayFromListN :: forall a. Prim a => Int -> [a] -> ByteArray
{-# INLINE byteArrayFromListN #-}
byteArrayFromListN n ys = createByteArray (n * sizeOfType @a) $ \marr ->
  let z ix# = if I# ix# == n
        then return ()
        else die "byteArrayFromListN" "list length less than specified size"
      f x k = GHC.Exts.oneShot $ \ix# -> if I# ix# < n
        then do
          writeByteArray marr (I# ix#) x
          k (ix# +# 1#)
        else die "byteArrayFromListN" "list length greater than specified size"
  in foldr f z ys 0#

unI# :: Int -> Int#
unI# (I# n#) = n#

-- | Copy a slice of an immutable byte array to a mutable byte array.
--
-- /Note:/ this function does not do bounds or overlap checking.
copyByteArray
  :: PrimMonad m
  => MutableByteArray (PrimState m) -- ^ destination array
  -> Int                            -- ^ offset into destination array
  -> ByteArray                      -- ^ source array
  -> Int                            -- ^ offset into source array
  -> Int                            -- ^ number of bytes to copy
  -> m ()
{-# INLINE copyByteArray #-}
copyByteArray (MutableByteArray dst#) doff (ByteArray src#) soff sz
  = primitive_ (copyByteArray# src# (unI# soff) dst# (unI# doff) (unI# sz))

-- | Copy a slice of a mutable byte array into another array. The two slices
-- may not overlap.
--
-- /Note:/ this function does not do bounds or overlap checking.
copyMutableByteArray
  :: PrimMonad m
  => MutableByteArray (PrimState m) -- ^ destination array
  -> Int                            -- ^ offset into destination array
  -> MutableByteArray (PrimState m) -- ^ source array
  -> Int                            -- ^ offset into source array
  -> Int                            -- ^ number of bytes to copy
  -> m ()
{-# INLINE copyMutableByteArray #-}
copyMutableByteArray (MutableByteArray dst#) doff
                     (MutableByteArray src#) soff sz
  = primitive_ (op src# (unI# soff) dst# (unI# doff) (unI# sz))
  where
#if MIN_VERSION_base(4,19,0)
    op = copyMutableByteArrayNonOverlapping#
#else
    op = copyMutableByteArray#
#endif

-- | Copy a slice of a byte array to an unmanaged pointer address. These must not
-- overlap. The offset and length are given in elements, not in bytes.
--
-- /Note:/ this function does not do bounds or overlap checking.
--
-- @since 0.7.1.0
copyByteArrayToPtr
  :: forall m a. (PrimMonad m, Prim a)
  => Ptr a -- ^ destination
  -> ByteArray -- ^ source array
  -> Int -- ^ offset into source array, interpreted as elements of type @a@
  -> Int -- ^ number of elements to copy
  -> m ()
{-# INLINE copyByteArrayToPtr #-}
copyByteArrayToPtr (Ptr dst#) (ByteArray src#) soff sz
  = primitive_ (copyByteArrayToAddr# src# (unI# soff *# siz#) dst# (unI# sz *# siz#))
  where
  siz# = sizeOfType# (Proxy :: Proxy a)

-- | Copy from an unmanaged pointer address to a byte array. These must not
-- overlap. The offset and length are given in elements, not in bytes.
--
-- /Note:/ this function does not do bounds or overlap checking.
copyPtrToMutableByteArray :: forall m a. (PrimMonad m, Prim a)
  => MutableByteArray (PrimState m) -- ^ destination array
  -> Int   -- ^ destination offset given in elements of type @a@
  -> Ptr a -- ^ source pointer
  -> Int   -- ^ number of elements
  -> m ()
{-# INLINE copyPtrToMutableByteArray #-}
copyPtrToMutableByteArray (MutableByteArray ba#) (I# doff#) (Ptr addr#) (I# n#) =
  primitive_ (copyAddrToByteArray# addr# ba# (doff# *# siz#) (n# *# siz#))
  where
  siz# = sizeOfType# (Proxy :: Proxy a)


-- | Copy a slice of a mutable byte array to an unmanaged pointer address.
-- These must not overlap. The offset and length are given in elements, not
-- in bytes.
--
-- /Note:/ this function does not do bounds or overlap checking.
--
-- @since 0.7.1.0
copyMutableByteArrayToPtr
  :: forall m a. (PrimMonad m, Prim a)
  => Ptr a -- ^ destination
  -> MutableByteArray (PrimState m) -- ^ source array
  -> Int -- ^ offset into source array, interpreted as elements of type @a@
  -> Int -- ^ number of elements to copy
  -> m ()
{-# INLINE copyMutableByteArrayToPtr #-}
copyMutableByteArrayToPtr (Ptr dst#) (MutableByteArray src#) soff sz
  = primitive_ (copyMutableByteArrayToAddr# src# (unI# soff *# siz#) dst# (unI# sz *# siz#))
  where
  siz# = sizeOfType# (Proxy :: Proxy a)

------
--- These latter two should be DEPRECATED
-----

-- | Copy a slice of a byte array to an unmanaged address. These must not
-- overlap.
--
-- Note: This function is just 'copyByteArrayToPtr' where @a@ is 'Word8'.
--
-- @since 0.6.4.0
copyByteArrayToAddr
  :: PrimMonad m
  => Ptr Word8 -- ^ destination
  -> ByteArray -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of bytes to copy
  -> m ()
{-# INLINE copyByteArrayToAddr #-}
copyByteArrayToAddr (Ptr dst#) (ByteArray src#) soff sz
  = primitive_ (copyByteArrayToAddr# src# (unI# soff) dst# (unI# sz))

-- | Copy a slice of a mutable byte array to an unmanaged address. These must
-- not overlap.
--
-- Note: This function is just 'copyMutableByteArrayToPtr' where @a@ is 'Word8'.
--
-- @since 0.6.4.0
copyMutableByteArrayToAddr
  :: PrimMonad m
  => Ptr Word8 -- ^ destination
  -> MutableByteArray (PrimState m) -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of bytes to copy
  -> m ()
{-# INLINE copyMutableByteArrayToAddr #-}
copyMutableByteArrayToAddr (Ptr dst#) (MutableByteArray src#) soff sz
  = primitive_ (copyMutableByteArrayToAddr# src# (unI# soff) dst# (unI# sz))

-- | Copy a slice of a mutable byte array into another, potentially
-- overlapping array.
moveByteArray
  :: PrimMonad m
  => MutableByteArray (PrimState m) -- ^ destination array
  -> Int                            -- ^ offset into destination array
  -> MutableByteArray (PrimState m) -- ^ source array
  -> Int                            -- ^ offset into source array
  -> Int                            -- ^ number of bytes to copy
  -> m ()
{-# INLINE moveByteArray #-}
moveByteArray (MutableByteArray dst#) doff
              (MutableByteArray src#) soff sz
  = primitive_ (copyMutableByteArray# src# (unI# soff) dst# (unI# doff) (unI# sz))

-- | Fill a slice of a mutable byte array with a value. The offset and length
-- are given in elements of type @a@ rather than in bytes.
--
-- /Note:/ this function does not do bounds checking.
setByteArray
  :: (Prim a, PrimMonad m)
  => MutableByteArray (PrimState m) -- ^ array to fill
  -> Int                            -- ^ offset into array
  -> Int                            -- ^ number of values to fill
  -> a                              -- ^ value to fill with
  -> m ()
{-# INLINE setByteArray #-}
setByteArray (MutableByteArray dst#) (I# doff#) (I# sz#) x
  = primitive_ (setByteArray# dst# doff# sz# x)

-- | Fill a slice of a mutable byte array with a byte.
--
-- /Note:/ this function does not do bounds checking.
fillByteArray
  :: PrimMonad m
  => MutableByteArray (PrimState m) -- ^ array to fill
  -> Int                            -- ^ offset into array
  -> Int                            -- ^ number of bytes to fill
  -> Word8                          -- ^ byte to fill with
  -> m ()
{-# INLINE fillByteArray #-}
fillByteArray = setByteArray

-- | Lexicographic comparison of equal-length slices into two byte arrays.
-- This wraps the @compareByteArrays#@ primop, which wraps @memcmp@.
compareByteArrays
  :: ByteArray -- ^ array A
  -> Int       -- ^ offset A, given in bytes
  -> ByteArray -- ^ array B
  -> Int       -- ^ offset B, given in bytes
  -> Int       -- ^ length of the slice, given in bytes
  -> Ordering
{-# INLINE compareByteArrays #-}
#if __GLASGOW_HASKELL__ >= 804
compareByteArrays (ByteArray ba1#) (I# off1#) (ByteArray ba2#) (I# off2#) (I# n#)
  = compare (I# (compareByteArrays# ba1# off1# ba2# off2# n#)) 0
#else
-- Emulate GHC 8.4's 'GHC.Prim.compareByteArrays#'
compareByteArrays (ByteArray ba1#) (I# off1#) (ByteArray ba2#) (I# off2#) (I# n#)
  = compare (fromCInt (unsafeDupablePerformIO (memcmp_ba_offs ba1# off1# ba2# off2# n))) 0
  where
    n = fromIntegral (I# n#) :: CSize
    fromCInt = fromIntegral :: CInt -> Int

foreign import ccall unsafe "primitive-memops.h hsprimitive_memcmp_offset"
  memcmp_ba_offs :: ByteArray# -> Int# -> ByteArray# -> Int# -> CSize -> IO CInt
#endif

-- | The empty 'ByteArray'.
emptyByteArray :: ByteArray
{-# NOINLINE emptyByteArray #-}
emptyByteArray = runST (newByteArray 0 >>= unsafeFreezeByteArray)

emptyByteArray# :: (# #) -> ByteArray#
{-# NOINLINE emptyByteArray# #-}
emptyByteArray# _ = case emptyByteArray of ByteArray arr# -> arr#

die :: String -> String -> a
die fun problem = error $ "Data.Primitive.ByteArray." ++ fun ++ ": " ++ problem

-- | Return a newly allocated array with the specified subrange of the
-- provided array. The provided array should contain the full subrange
-- specified by the two Ints, but this is not checked.
cloneByteArray
  :: ByteArray -- ^ source array
  -> Int       -- ^ offset into destination array
  -> Int       -- ^ number of bytes to copy
  -> ByteArray
{-# INLINE cloneByteArray #-}
cloneByteArray src off n = createByteArray n $ \dst ->
  copyByteArray dst 0 src off n

-- | Return a newly allocated mutable array with the specified subrange of
-- the provided mutable array. The provided mutable array should contain the
-- full subrange specified by the two Ints, but this is not checked.
cloneMutableByteArray :: PrimMonad m
  => MutableByteArray (PrimState m) -- ^ source array
  -> Int -- ^ offset into destination array
  -> Int -- ^ number of bytes to copy
  -> m (MutableByteArray (PrimState m))
{-# INLINE cloneMutableByteArray #-}
cloneMutableByteArray src off n = do
  dst <- newByteArray n
  copyMutableByteArray dst 0 src off n
  return dst

-- | Execute the monadic action and freeze the resulting array.
--
-- > runByteArray m = runST $ m >>= unsafeFreezeByteArray
runByteArray
  :: (forall s. ST s (MutableByteArray s))
  -> ByteArray
runByteArray m = ByteArray (runByteArray# m)

runByteArray#
  :: (forall s. ST s (MutableByteArray s))
  -> ByteArray#
runByteArray# m = case runRW# $ \s ->
  case unST m s of { (# s', MutableByteArray mary# #) ->
  unsafeFreezeByteArray# mary# s'} of (# _, ary# #) -> ary#

unST :: ST s a -> State# s -> (# State# s, a #)
unST (GHCST.ST f) = f

-- Create an uninitialized array of the given size in bytes, apply the function
-- to it, and freeze the result.
--
-- /Note:/ this function does not check if the input is non-negative.
--
-- @since FIXME
createByteArray :: Int -> (forall s. MutableByteArray s -> ST s ()) -> ByteArray
{-# INLINE createByteArray #-}
createByteArray 0 _ = ByteArray (emptyByteArray# (# #))
createByteArray n f = runByteArray $ do
  marr <- newByteArray n
  f marr
  pure marr

{- $charElementAccess
GHC provides two sets of element accessors for 'Char'. One set faithfully
represents 'Char' as 32-bit words using UTF-32. The other set represents
'Char' as 8-bit words using Latin-1 (ISO-8859-1), and the write operation
has undefined behavior for codepoints outside of the ASCII and Latin-1
blocks. The 'Prim' instance for 'Char' uses the UTF-32 set of operators.
-}

-- | Read an 8-bit element from the byte array, interpreting it as a
-- Latin-1-encoded character. The offset is given in bytes.
--
-- /Note:/ this function does not do bounds checking.
readCharArray :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> m Char
{-# INLINE readCharArray #-}
readCharArray (MutableByteArray arr#) (I# i#) = primitive
  (\s0 -> case readCharArray# arr# i# s0 of
    (# s1, c #) -> (# s1, C# c #)
  )

-- | Write a character to the byte array, encoding it with Latin-1 as
-- a single byte. Behavior is undefined for codepoints outside of the
-- ASCII and Latin-1 blocks. The offset is given in bytes.
--
-- /Note:/ this function does not do bounds checking.
writeCharArray
  :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> Char -> m ()
{-# INLINE writeCharArray #-}
writeCharArray (MutableByteArray arr#) (I# i#) (C# c)
  = primitive_ (writeCharArray# arr# i# c)

-- | Read an 8-bit element from the byte array, interpreting it as a
-- Latin-1-encoded character. The offset is given in bytes.
--
-- /Note:/ this function does not do bounds checking.
indexCharArray :: ByteArray -> Int -> Char
{-# INLINE indexCharArray #-}
indexCharArray (ByteArray arr#) (I# i#) = C# (indexCharArray# arr# i#)
