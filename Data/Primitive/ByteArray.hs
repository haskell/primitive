{-# LANGUAGE CPP, MagicHash, UnboxedTuples, UnliftedFFITypes, DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Data.Primitive.ByteArray
-- Copyright   : (c) Roman Leshchinskiy 2009-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Primitive operations on ByteArrays
--

module Data.Primitive.ByteArray (
  -- * Types
  ByteArray(..), MutableByteArray(..), ByteArray#, MutableByteArray#,

  -- * Allocation
  newByteArray, newPinnedByteArray, newAlignedPinnedByteArray,

  -- * Element access
  readByteArray, writeByteArray, indexByteArray,

  -- * Freezing and thawing
  unsafeFreezeByteArray, unsafeThawByteArray,

  -- * Block operations
  copyByteArray, copyMutableByteArray, moveByteArray,
  setByteArray, fillByteArray, resizeMutableByteArray, shrinkMutableByteArray,
  copyByteArrayToAddr, copyMutableByteArrayToAddr, copyMutableByteArrayFromAddr,

  -- * Information
  sizeofByteArray, sizeofMutableByteArray, sameMutableByteArray,
  byteArrayContents, mutableByteArrayContents,
  isByteArrayPinned, isMutableByteArrayPinned,
  sameByteArray,

  -- * Prefetch
  -- | Check <http://hackage.haskell.org/package/ghc-prim-0.3.1.0/docs/GHC-Prim.html#Prefetch> for more information.

  prefetchByteArray0, prefetchByteArray1, prefetchByteArray2, prefetchByteArray3,
  prefetchMutableByteArray0, prefetchMutableByteArray1, prefetchMutableByteArray2, prefetchMutableByteArray3

) where

import Control.Monad.Primitive
import Data.Primitive.Types

import Foreign.C.Types
import Data.Word ( Word8 )
import GHC.Base ( Int(..) )
import GHC.Prim
#if __GLASGOW_HASKELL__ >= 706
    hiding (setByteArray#)
#endif

import Data.Typeable ( Typeable )
import Data.Data ( Data(..) )
import Data.Primitive.Internal.Compat ( isTrue#, mkNoRepType )

-- | Byte arrays
data ByteArray = ByteArray ByteArray# deriving ( Typeable )

instance Eq ByteArray where
    {-# INLINE (==) #-}
    paA@(ByteArray baA#) == paB@(ByteArray baB#) =
        sameByteArray paA paB || (
            let sizA = sizeofByteArray paA
                sizB = sizeofByteArray paB
            in sizA == sizB && c_memcmp baA# baB# (fromIntegral sizA) == 0)

instance Ord ByteArray where
    {-# INLINE compare #-}
    paA@(ByteArray baA#) `compare` paB@(ByteArray baB#)
        | sameByteArray paA paB = EQ
        | otherwise =
            let sizA = sizeofByteArray paA
                sizB = sizeofByteArray paB
                r = c_memcmp baA# baB# (fromIntegral $ min sizA sizB)
            in case r `compare` 0 of
                EQ  -> sizA `compare` sizB
                x  -> x

foreign import ccall unsafe "cstring.h memcmp" c_memcmp :: ByteArray# -> ByteArray# -> CInt -> CInt

-- | Mutable byte arrays associated with a primitive state token
data MutableByteArray s = MutableByteArray (MutableByteArray# s)
                                        deriving( Typeable )

-- | Create a new mutable byte array of the specified size.
newByteArray :: PrimMonad m => Int -> m (MutableByteArray (PrimState m))
{-# INLINE newByteArray #-}
newByteArray (I# n#)
  = primitive (\s# -> case newByteArray# n# s# of
                        (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #))

-- | Create a /pinned/ byte array of the specified size. The garbage collector
-- is guaranteed not to move it.
newPinnedByteArray :: PrimMonad m => Int -> m (MutableByteArray (PrimState m))
{-# INLINE newPinnedByteArray #-}
newPinnedByteArray (I# n#)
  = primitive (\s# -> case newPinnedByteArray# n# s# of
                        (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #))

-- | Create a /pinned/ byte array of the specified size and with the give
-- alignment. The garbage collector is guaranteed not to move it.
newAlignedPinnedByteArray
  :: PrimMonad m => Int -> Int -> m (MutableByteArray (PrimState m))
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

-- | Yield a pointer to the array's data. This operation is only safe on
-- /pinned/ byte arrays allocated by 'newPinnedByteArray' or
-- 'newAlignedPinnedByteArray'.
mutableByteArrayContents :: MutableByteArray s -> Addr
{-# INLINE mutableByteArrayContents #-}
mutableByteArrayContents (MutableByteArray arr#)
  = Addr (byteArrayContents# (unsafeCoerce# arr#))

-- | Check if the two arrays refer to the same memory block.
sameMutableByteArray :: MutableByteArray s -> MutableByteArray s -> Bool
{-# INLINE sameMutableByteArray #-}
sameMutableByteArray (MutableByteArray arr#) (MutableByteArray brr#)
  = isTrue# (sameMutableByteArray# arr# brr#)

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
readByteArray
  :: (Prim a, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> m a
{-# INLINE readByteArray #-}
readByteArray (MutableByteArray arr#) (I# i#)
  = primitive (readByteArray# arr# i#)

-- | Write a primitive value to the byte array. The offset is given in
-- elements of type @a@ rather than in bytes.
writeByteArray
  :: (Prim a, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> a -> m ()
{-# INLINE writeByteArray #-}
writeByteArray (MutableByteArray arr#) (I# i#) x
  = primitive_ (writeByteArray# arr# i# x)

#if __GLASGOW_HASKELL__ >= 702
unI# :: Int -> Int#
unI# (I# n#) = n#
#endif

-- | Copy a slice of an immutable byte array to a mutable byte array.
copyByteArray
  :: PrimMonad m => MutableByteArray (PrimState m)
                                        -- ^ destination array
                 -> Int                 -- ^ offset into destination array
                 -> ByteArray           -- ^ source array
                 -> Int                 -- ^ offset into source array
                 -> Int                 -- ^ number of bytes to copy
                 -> m ()
{-# INLINE copyByteArray #-}
copyByteArray (MutableByteArray dst#) doff (ByteArray src#) soff sz
#if __GLASGOW_HASKELL__ >= 702
  = primitive_ (copyByteArray# src# (unI# soff) dst# (unI# doff) (unI# sz))
#else
  = unsafePrimToPrim
  $ memcpy_ba dst# (fromIntegral doff) src# (fromIntegral soff)
                 (fromIntegral sz)
#endif

-- | Copy a slice of a mutable byte array into another array. The two slices
-- may not overlap.
copyMutableByteArray
  :: PrimMonad m => MutableByteArray (PrimState m)
                                        -- ^ destination array
                 -> Int                 -- ^ offset into destination array
                 -> MutableByteArray (PrimState m)
                                        -- ^ source array
                 -> Int                 -- ^ offset into source array
                 -> Int                 -- ^ number of bytes to copy
                 -> m ()
{-# INLINE copyMutableByteArray #-}
copyMutableByteArray (MutableByteArray dst#) doff
                     (MutableByteArray src#) soff sz
#if __GLASGOW_HASKELL__ >= 702
  = primitive_ (copyMutableByteArray# src# (unI# soff) dst# (unI# doff) (unI# sz))
#else
  = unsafePrimToPrim
  $ memcpy_mba dst# (fromIntegral doff) src# (fromIntegral soff)
                    (fromIntegral sz)
#endif

-- | Copy a slice of an immutable primitive array to an address.
-- The offset and length are given in elements of type @a@.
copyByteArrayToAddr :: PrimMonad m
              => Addr                             -- ^ destination pointer
              -> ByteArray                        -- ^ source array
              -> Int                              -- ^ offset into source array
              -> Int                              -- ^ number of prims to copy
              -> m ()
{-# INLINE copyByteArrayToAddr #-}
copyByteArrayToAddr (Addr dst#) (ByteArray src#) soff@(I# soff#) sz@(I# n#) =
#if __GLASGOW_HASKELL__ >= 780
    primitive_ (copyByteArrayToAddr# src# soff# dst# n#)
#else
    unsafePrimToPrim
        $ memcpy_ba_to_addr dst# 0 src# (fromIntegral soff) (fromIntegral sz)
#endif

-- | Copy a slice of an mutable primitive array to an address.
-- The offset and length are given in elements of type @a@.
--
copyMutableByteArrayToAddr :: PrimMonad m
              => Addr                             -- ^ destination pointer
              -> MutableByteArray (PrimState m)   -- ^ source array
              -> Int                              -- ^ offset into source array
              -> Int                              -- ^ number of prims to copy
              -> m ()
{-# INLINE copyMutableByteArrayToAddr #-}
copyMutableByteArrayToAddr (Addr dst#) (MutableByteArray src#) soff@(I# soff#) sz@(I# n#) =
#if __GLASGOW_HASKELL__ >= 780
    primitive_ (copyMutableByteArrayToAddr# src# soff# dst# n#)
#else
    unsafePrimToPrim
        $ memcpy_mba_to_addr dst# 0 src# (fromIntegral soff) (fromIntegral sz)
#endif

-- | Copy a slice of an mutable primitive array from an address.
-- The offset and length are given in elements of type @a@.
--
copyMutableByteArrayFromAddr :: PrimMonad m
              => MutableByteArray (PrimState m)   -- ^ destination array
              -> Int                              -- ^ offset into destination array
              -> Addr                             -- ^ source pointer
              -> Int                              -- ^ number of prims to copy
              -> m ()
{-# INLINE copyMutableByteArrayFromAddr #-}
copyMutableByteArrayFromAddr (MutableByteArray dst#) doff@(I# doff#) (Addr src#) sz@(I# n#) =
#if __GLASGOW_HASKELL__ >= 780
    primitive_ (copyAddrToByteArray# src# dst# doff# n#)
#else
    unsafePrimToPrim
        $ memcpy_mba_from_addr dst# (fromIntegral doff) src# 0 (fromIntegral sz)
#endif

-- | Copy a slice of a mutable byte array into another, potentially
-- overlapping array.
moveByteArray
  :: PrimMonad m => MutableByteArray (PrimState m)
                                        -- ^ destination array
                 -> Int                 -- ^ offset into destination array
                 -> MutableByteArray (PrimState m)
                                        -- ^ source array
                 -> Int                 -- ^ offset into source array
                 -> Int                 -- ^ number of bytes to copy
                 -> m ()
{-# INLINE moveByteArray #-}
moveByteArray (MutableByteArray dst#) doff
              (MutableByteArray src#) soff sz
  = unsafePrimToPrim
  $ memmove_mba dst# (fromIntegral doff) src# (fromIntegral soff)
                     (fromIntegral sz)

-- | Fill a slice of a mutable byte array with a value. The offset and length
-- are given in elements of type @a@ rather than in bytes.
setByteArray
  :: (Prim a, PrimMonad m) => MutableByteArray (PrimState m) -- ^ array to fill
                           -> Int                 -- ^ offset into array
                           -> Int                 -- ^ number of values to fill
                           -> a                   -- ^ value to fill with
                           -> m ()
{-# INLINE setByteArray #-}
setByteArray (MutableByteArray dst#) (I# doff#) (I# sz#) x
  = primitive_ (setByteArray# dst# doff# sz# x)

-- | Fill a slice of a mutable byte array with a byte.
fillByteArray
  :: PrimMonad m => MutableByteArray (PrimState m)
                                        -- ^ array to fill
                 -> Int                 -- ^ offset into array
                 -> Int                 -- ^ number of bytes to fill
                 -> Word8               -- ^ byte to fill with
                 -> m ()
{-# INLINE fillByteArray #-}
fillByteArray = setByteArray

-- | Resize a byte array using 'resizeMutableByteArray#' if available.
--
-- To avoid undefined behaviour, the original 'MutablePrimArray' shall not be accessed anymore.
--
resizeMutableByteArray :: PrimMonad m
                       => MutableByteArray (PrimState m)
                       -> Int
                       -> m (MutableByteArray (PrimState m))
resizeMutableByteArray mba@(MutableByteArray mba#) sz@(I# i#) = do
#if __GLASGOW_HASKELL__ >= 710
    primitive (\ s# ->
            let !(# s'#, mba'# #) = resizeMutableByteArray# mba# i# s#
            in (# s'#, (MutableByteArray mba'#) #)
       )
#else
    mba' <- newByteArray sz
    copyMutableByteArray mba' 0 mba 0 (min sz (sizeofMutableByteArray mba))
    return mba'
#endif
{-# INLINE resizeMutableByteArray #-}

-- | Shrink a byte array using 'shrinkMutableByteArray#' if available.
--
-- The new size argument must be less than or equal to the current size, but it's not checked.
--
shrinkMutableByteArray :: PrimMonad m
                       => MutableByteArray (PrimState m)
                       -> Int
                       -> m ()
shrinkMutableByteArray (MutableByteArray mba#) (I# i#) =
#if __GLASGOW_HASKELL__ >= 710
    primitive_ (shrinkMutableByteArray# mba# i#)
#else
    return ()
#endif
{-# INLINE shrinkMutableByteArray #-}


#if __GLASGOW_HASKELL__ < 702
foreign import ccall unsafe "primitive-memops.h hsprimitive_memcpy"
  memcpy_mba :: MutableByteArray# s -> CInt
             -> MutableByteArray# s -> CInt
             -> CSize -> IO ()

foreign import ccall unsafe "primitive-memops.h hsprimitive_memcpy"
  memcpy_ba :: MutableByteArray# s -> CInt
            -> ByteArray# -> CInt
            -> CSize -> IO ()
#endif

#if __GLASGOW_HASKELL__ < 780
foreign import ccall unsafe "primitive-memops.h hsprimitive_memcpy"
  memcpy_mba_to_addr :: Addr# -> CInt
                     -> MutableByteArray# s -> CInt
                     -> CSize -> IO ()

foreign import ccall unsafe "primitive-memops.h hsprimitive_memcpy"
  memcpy_mba_from_addr :: MutableByteArray# s -> CInt
                       -> Addr# -> CInt
                       -> CSize -> IO ()

foreign import ccall unsafe "primitive-memops.h hsprimitive_memcpy"
  memcpy_ba_to_addr :: Addr# -> CInt
                    -> ByteArray# -> CInt
                    -> CSize -> IO ()
#endif

foreign import ccall unsafe "primitive-memops.h hsprimitive_memmove"
  memmove_mba :: MutableByteArray# s -> CInt
              -> MutableByteArray# s -> CInt
              -> CSize -> IO ()

instance Data ByteArray where
  toConstr _ = error "toConstr"
  gunfold _ _ = error "gunfold"
  dataTypeOf _ = mkNoRepType "Data.Primitive.ByteArray.ByteArray"

instance Typeable s => Data (MutableByteArray s) where
  toConstr _ = error "toConstr"
  gunfold _ _ = error "gunfold"
  dataTypeOf _ = mkNoRepType "Data.Primitive.ByteArray.MutableByteArray"

--------------------------------------------------------------------------------

prefetchByteArray0, prefetchByteArray1, prefetchByteArray2, prefetchByteArray3
    :: PrimMonad m => ByteArray -> Int -> m ()
{-# INLINE prefetchByteArray0 #-}
{-# INLINE prefetchByteArray1 #-}
{-# INLINE prefetchByteArray2 #-}
{-# INLINE prefetchByteArray3 #-}
#if __GLASGOW_HASKELL__ >= 780
prefetchByteArray0 (ByteArray ba#) (I# i#) =
    primitive_ (prefetchByteArray0# ba# i#)
prefetchByteArray1 (ByteArray ba#) (I# i#) =
    primitive_ (prefetchByteArray1# ba# i#)
prefetchByteArray2 (ByteArray ba#) (I# i#) =
    primitive_ (prefetchByteArray2# ba# i#)
prefetchByteArray3 (ByteArray ba#) (I# i#) =
    primitive_ (prefetchByteArray3# ba# i#)
#else
prefetchByteArray0 _ _ = return ()
prefetchByteArray1 _ _ = return ()
prefetchByteArray2 _ _ = return ()
prefetchByteArray3 _ _ = return ()
#endif



prefetchMutableByteArray0, prefetchMutableByteArray1, prefetchMutableByteArray2, prefetchMutableByteArray3
    :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> m ()
{-# INLINE prefetchMutableByteArray0 #-}
{-# INLINE prefetchMutableByteArray1 #-}
{-# INLINE prefetchMutableByteArray2 #-}
{-# INLINE prefetchMutableByteArray3 #-}
#if __GLASGOW_HASKELL__ >= 780
prefetchMutableByteArray0 (MutableByteArray mba#) (I# i#) =
    primitive_ (prefetchMutableByteArray0# mba# i#)
prefetchMutableByteArray1 (MutableByteArray mba#) (I# i#) =
    primitive_ (prefetchMutableByteArray1# mba# i#)
prefetchMutableByteArray2 (MutableByteArray mba#) (I# i#) =
    primitive_ (prefetchMutableByteArray2# mba# i#)
prefetchMutableByteArray3 (MutableByteArray mba#) (I# i#) =
    primitive_ (prefetchMutableByteArray3# mba# i#)
#else
prefetchMutableByteArray0 _ _ = return ()
prefetchMutableByteArray1 _ _ = return ()
prefetchMutableByteArray2 _ _ = return ()
prefetchMutableByteArray3 _ _ = return ()
#endif

--------------------------------------------------------------------------------

-- | Check if the two immutable byte arrays refer to the same memory block.
sameByteArray :: ByteArray -> ByteArray -> Bool
{-# INLINE sameByteArray #-}
sameByteArray (ByteArray ba1#) (ByteArray ba2#) =
    isTrue# (sameMutableByteArray# (unsafeCoerce# ba1#) (unsafeCoerce# ba2#))

-- | Check if a byte array is pinned.
--
isByteArrayPinned :: ByteArray -> Bool
{-# INLINE isByteArrayPinned #-}
isByteArrayPinned (ByteArray ba#) =
    c_is_byte_array_pinned ba# > 0

-- | Check if a mutable byte array is pinned.
--
isMutableByteArrayPinned :: MutableByteArray s -> Bool
{-# INLINE isMutableByteArrayPinned #-}
isMutableByteArrayPinned (MutableByteArray mba#) =
    c_is_mutable_byte_array_pinned mba# > 0

foreign import ccall unsafe "hsprimitive_is_byte_array_pinned"
    c_is_byte_array_pinned :: ByteArray# -> CInt

foreign import ccall unsafe "hsprimitive_is_byte_array_pinned"
    c_is_mutable_byte_array_pinned :: MutableByteArray# s -> CInt
