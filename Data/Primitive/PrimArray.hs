{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

{-|
Module      : Data.Primitive.PrimArray
Description : Primitive arrays tagged with element type
Copyright   : (c) Winter, 2017
License     : BSD3

Maintainer  : drkoster@qq.com, libraries@haskell.org
Stability   : experimental
Portability : non-portable

This module provide primitive arrays tagged with their element
type. Offset, length, etc. are based on elements rather than bytes,
and operations are __not__ bound checked.

You can define new primitive array by defining 'Prim' instances, but
since GHC heap arrays are word aligned, it is recommended that users do
not create a 'PrimArray' with a type whose alignment does not divide
evenly into the machine word size. For such types, you have to use
'newAlignedPinnedPrimArray' to ensure that alignment is respected.

Here is an example of an RGB pixel 'Prim' instance.

@
  data Pixel = Pixel Word8 Word8 Word8 -- you may want to unpack these
  instance Prim Pixel where
    sizeOf# _ = 3#
    alignment# _ = 1#
    indexByteArray# ba# i# = Pixel
      (indexByteArray# ba# i#)
      (indexByteArray# ba# (i# +# 1#))
      (indexByteArray# ba# (i# +# 2#))
    ...
@

Now you can use 'PrimArray' 'Pixel' with either this module or @Data.Array@/@Data.Vector@.
-}


module Data.Primitive.PrimArray (
  -- * Types
  PrimArray(..), MutablePrimArray(..),

  -- * Allocation
  newPrimArray, newPinnedPrimArray, newAlignedPinnedPrimArray,

  -- * Element access
  readPrimArray, writePrimArray, indexPrimArray,

  -- * Freezing and thawing
  unsafeFreezePrimArray, unsafeThawPrimArray,

  -- * Block operations
  copyPrimArray, copyMutablePrimArray, movePrimArray,
  setPrimArray, resizeMutablePrimArray, shrinkMutablePrimArray,
  copyPrimArrayToPtr, copyMutablePrimArrayToPtr, copyMutablePrimArrayFromPtr,

  -- * Information
  sizeofPrimArray, sizeofMutablePrimArray, sameMutablePrimArray,
  primArrayContents, mutablePrimArrayContents, withPrimArrayContents, withMutablePrimArrayContents,
  isPrimArrayPinned, isMutablePrimArrayPinned,
  samePrimArray,

  -- * Prefetch
  -- | Check <http://hackage.haskell.org/package/ghc-prim-0.3.1.0/docs/GHC-Prim.html#Prefetch> for more information.

  prefetchPrimArray0, prefetchPrimArray1, prefetchPrimArray2, prefetchPrimArray3,
  prefetchMutablePrimArray0, prefetchMutablePrimArray1, prefetchMutablePrimArray2, prefetchMutablePrimArray3

) where


import Control.Monad.Primitive
import Control.Monad (liftM)
import Data.Primitive
import Data.Typeable
import Data.Data
import GHC.Ptr (Ptr(..))
import GHC.Base ( Int(..) )
import GHC.Word (Word8)
import GHC.Prim
import Data.Primitive.Internal.Compat ( isTrue# )

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid (mappend)
#endif

-- | Primitive array tagged with element type @a@.
--
newtype PrimArray a = PrimArray ByteArray
    deriving (Typeable, Data)

instance (Prim a, Eq a) => Eq (PrimArray a) where
    {-# INLINE (==) #-}
    (PrimArray baA) == (PrimArray baB) = baA == baB

instance {-# OVERLAPPABLE #-} (Prim a, Ord a) => Ord (PrimArray a) where
    {-# INLINE compare #-}
    paA `compare` paB
        | paA `samePrimArray` paB = EQ
        | otherwise = go 0
      where
        !endA = sizeofPrimArray paA
        !endB = sizeofPrimArray paB
        end = endA `min` endB
        go !i | i >= end  = endA `compare` endB
              | otherwise = indexPrimArray paA i `compare` indexPrimArray paB i `mappend` go (i+1)
instance {-# OVERLAPPING #-} Ord (PrimArray Word8) where
    {-# INLINE compare #-}
    (PrimArray baA) `compare` (PrimArray baB) = baA `compare` baB


-- | Mutable primitive array tagged with element type @a@.
--
newtype MutablePrimArray s a = MutablePrimArray (MutableByteArray s)
    deriving (Typeable, Data)

-- | Create a new mutable primitive array of the specified size.
newPrimArray :: forall m a . (PrimMonad m, Prim a) => Int -> m (MutablePrimArray (PrimState m) a)
{-# INLINE newPrimArray #-}
newPrimArray n = MutablePrimArray `liftM` newByteArray (n*siz)
  where siz = sizeOf (undefined :: a)

-- | Create a /pinned/ byte array of the specified size and respect the primitive type's
-- alignment. The garbage collector is guaranteed not to move it.
newPinnedPrimArray :: forall m a. (PrimMonad m, Prim a) => Int -> m (MutablePrimArray (PrimState m) a)
{-# INLINE newPinnedPrimArray #-}
newPinnedPrimArray n = MutablePrimArray `liftM` newAlignedPinnedByteArray (n*siz) align
  where siz = sizeOf (undefined :: a)
        align = alignment (undefined :: a)

-- | Create a /pinned/ primitive array of the specified size and respect given
-- alignment. The garbage collector is guaranteed not to move it.
newAlignedPinnedPrimArray
  :: forall m a. (PrimMonad m, Prim a) => Int -> Int -> m (MutablePrimArray (PrimState m) a)
{-# INLINE newAlignedPinnedPrimArray #-}
newAlignedPinnedPrimArray n align = MutablePrimArray `liftM` newAlignedPinnedByteArray (n*siz) align
  where siz = sizeOf (undefined :: a)

-- | Yield a pointer to the array's data.
-- This operation is only safe on /pinned/ primitive arrays allocated by 'newPinnedPrimArray' or
-- 'newAlignedPinnedPrimArray', and you have to ensure the 'PrimArray' outlives the 'Ptr'.
--
primArrayContents :: PrimArray a -> Ptr a
{-# INLINE primArrayContents #-}
primArrayContents (PrimArray ba) =
    let !(Addr addr#) = byteArrayContents ba in Ptr addr#

-- | Yield a pointer to the array's data.
--
-- This operation is only safe on /pinned/ primitive arrays allocated by 'newPinnedPrimArray' or
-- 'newAlignedPinnedPrimArray'. and you have to ensure the 'PrimArray' outlives the 'Ptr'.
--
mutablePrimArrayContents :: MutablePrimArray s a -> Ptr a
{-# INLINE mutablePrimArrayContents #-}
mutablePrimArrayContents (MutablePrimArray mba) =
    let !(Addr addr#) = mutableByteArrayContents mba in Ptr addr#

-- | Yield a pointer to the array's data and do computation with it.
--
-- This operation is only safe on /pinned/ primitive arrays allocated by 'newPinnedPrimArray' or
-- 'newAlignedPinnedPrimArray'.
--
withPrimArrayContents :: PrimArray a -> (Ptr a -> IO b) -> IO b
{-# INLINE withPrimArrayContents #-}
withPrimArrayContents (PrimArray ba) f = do
    let !(Addr addr#) = byteArrayContents ba
        ptr = Ptr addr#
    b <- f ptr
    touch ba
    return b

-- | Yield a pointer to the array's data and do computation with it.
--
-- This operation is only safe on /pinned/ primitive arrays allocated by 'newPinnedPrimArray' or
-- 'newAlignedPinnedPrimArray'.
--
withMutablePrimArrayContents :: MutablePrimArray RealWorld a -> (Ptr a -> IO b) -> IO b
{-# INLINE withMutablePrimArrayContents #-}
withMutablePrimArrayContents (MutablePrimArray mba) f = do
    let !(Addr addr#) = mutableByteArrayContents mba
        ptr = Ptr addr#
    b <- f ptr
    touch mba
    return b

-- | Check if the two arrays refer to the same memory block.
sameMutablePrimArray :: MutablePrimArray s a -> MutablePrimArray s a -> Bool
{-# INLINE sameMutablePrimArray #-}
sameMutablePrimArray (MutablePrimArray mbaA) (MutablePrimArray mbaB) = sameMutableByteArray mbaA mbaB

-- | Convert a mutable primitive array to an immutable one without copying. The
-- array should not be modified after the conversion.
unsafeFreezePrimArray
  :: (PrimMonad m) => MutablePrimArray (PrimState m) a -> m (PrimArray a)
{-# INLINE unsafeFreezePrimArray #-}
unsafeFreezePrimArray (MutablePrimArray mba) = PrimArray `liftM` unsafeFreezeByteArray mba

-- | Convert an immutable primitive array to a mutable one without copying. The
-- original array should not be used after the conversion.
unsafeThawPrimArray
  :: (PrimMonad m) => PrimArray a -> m (MutablePrimArray (PrimState m) a)
{-# INLINE unsafeThawPrimArray #-}
unsafeThawPrimArray (PrimArray ba) = MutablePrimArray `liftM` unsafeThawByteArray ba

-- | Size of the primitive array.
sizeofPrimArray :: forall a . (Prim a) => PrimArray a -> Int
{-# INLINE sizeofPrimArray #-}
sizeofPrimArray (PrimArray ba) = sizeofByteArray ba `quot` siz
  where siz = sizeOf (undefined :: a)

-- | Size of the mutable primitive array.
sizeofMutablePrimArray :: forall m a . (PrimMonad m, Prim a) => MutablePrimArray (PrimState m) a -> m Int
{-# INLINE sizeofMutablePrimArray #-}
sizeofMutablePrimArray (MutablePrimArray mba) =
#if MIN_VERSION_ghc_prim(0,5,0)
    let getSizeofMutableByteArray (MutableByteArray mba#) = primitive (\ s# ->
            let !(# s'#, l# #) = getSizeofMutableByteArray# mba# s#
            in (# s'#, (I# l#) #))
    in (`quot` siz) `liftM` getSizeofMutableByteArray mba
#else
    return (sizeofMutableByteArray mba `quot` siz)
#endif
  where
    siz = sizeOf (undefined :: a)

-- | Read a primitive value from the primitive array. The offset is given in
-- elements of type @a@.
indexPrimArray :: Prim a => PrimArray a -> Int -> a
{-# INLINE indexPrimArray #-}
indexPrimArray (PrimArray ba) = indexByteArray ba

-- | Read a primitive value from the primitive array. The offset is given in
-- elements of type @a@.
readPrimArray
  :: forall m a. (PrimMonad m, Prim a) => MutablePrimArray (PrimState m) a -> Int -> m a
{-# INLINE readPrimArray #-}
readPrimArray (MutablePrimArray mba) = readByteArray mba

-- | Write a primitive value to the primitive array. The offset is given in
-- elements of type @a@.
writePrimArray
  :: forall m a. (PrimMonad m, Prim a) => MutablePrimArray (PrimState m) a -> Int -> a -> m ()
{-# INLINE writePrimArray #-}
writePrimArray (MutablePrimArray mba) = writeByteArray mba

-- | Copy a slice of an immutable primitive array to a mutable primitive array.
-- The offset and length are given in elements of type @a@.
copyPrimArray :: forall m a. (PrimMonad m, Prim a)
              => MutablePrimArray (PrimState m) a -- ^ destination array
              -> Int                              -- ^ offset into destination array
              -> PrimArray a                      -- ^ source array
              -> Int                              -- ^ offset into source array
              -> Int                              -- ^ number of prims to copy
              -> m ()
{-# INLINE copyPrimArray #-}
copyPrimArray (MutablePrimArray dst) doff (PrimArray src) soff n =
    copyByteArray dst (doff*siz) src (soff*siz) (n*siz)
  where siz = sizeOf (undefined :: a)

-- | Copy a slice of an immutable primitive array to an address.
-- The offset and length are given in elements of type @a@.
copyPrimArrayToPtr :: forall m a. (PrimMonad m, Prim a)
              => Ptr a                            -- ^ destination pointer
              -> PrimArray a                      -- ^ source array
              -> Int                              -- ^ offset into source array
              -> Int                              -- ^ number of prims to copy
              -> m ()
{-# INLINE copyPrimArrayToPtr #-}
copyPrimArrayToPtr (Ptr addr#) (PrimArray ba) soff sz =
    copyByteArrayToAddr (Addr addr#) ba (soff * siz) (sz * siz)
  where siz = sizeOf (undefined :: a)

-- | Copy a slice of an mutable primitive array to an address.
-- The offset and length are given in elements of type @a@.
--
copyMutablePrimArrayToPtr :: forall m a. (PrimMonad m, Prim a)
              => Ptr a                            -- ^ destination pointer
              -> MutablePrimArray (PrimState m) a -- ^ source array
              -> Int                              -- ^ offset into source array
              -> Int                              -- ^ number of prims to copy
              -> m ()
{-# INLINE copyMutablePrimArrayToPtr #-}
copyMutablePrimArrayToPtr (Ptr addr#) (MutablePrimArray mba) soff sz =
    copyMutableByteArrayToAddr (Addr addr#) mba (soff * siz) (sz * siz)
  where siz = sizeOf (undefined :: a)

-- | Copy a slice of an mutable primitive array from an address.
-- The offset and length are given in elements of type @a@.
--
copyMutablePrimArrayFromPtr :: forall m a. (PrimMonad m, Prim a)
              => MutablePrimArray (PrimState m) a -- ^ destination array
              -> Int                              -- ^ offset into destination array
              -> Ptr a                            -- ^ source pointer
              -> Int                              -- ^ number of prims to copy
              -> m ()
{-# INLINE copyMutablePrimArrayFromPtr #-}
copyMutablePrimArrayFromPtr (MutablePrimArray mba) doff (Ptr addr#) sz =
    copyMutableByteArrayFromAddr mba (doff * siz) (Addr addr#) (sz * siz)
  where siz = sizeOf (undefined :: a)

-- | Copy a slice of a mutable primitive array into another array. The two slices
-- may not overlap.
-- The offset and length are given in elements of type @a@.
copyMutablePrimArray :: forall m a. (PrimMonad m, Prim a)
                     => MutablePrimArray (PrimState m) a -- ^ destination array
                     -> Int                              -- ^ offset into destination array
                     -> MutablePrimArray (PrimState m) a -- ^ source array
                     -> Int                              -- ^ offset into source array
                     -> Int                              -- ^ number of prims to copy
                     -> m ()
{-# INLINE copyMutablePrimArray #-}
copyMutablePrimArray (MutablePrimArray dst) doff (MutablePrimArray src) soff n =
    copyMutableByteArray dst (doff*siz) src (soff*siz) (n*siz)
  where siz = sizeOf (undefined :: a)

-- | Copy a slice of a mutable primitive array into another, potentially
-- overlapping array.
-- The offset and length are given in elements of type @a@.
movePrimArray :: forall m a. (PrimMonad m, Prim a)
              => MutablePrimArray (PrimState m) a -- ^ destination array
              -> Int                              -- ^ offset into destination array
              -> MutablePrimArray (PrimState m) a -- ^ source array
              -> Int                              -- ^ offset into source array
              -> Int                              -- ^ number of prims to copy
              -> m ()
{-# INLINE movePrimArray #-}
movePrimArray (MutablePrimArray dst) doff (MutablePrimArray src) soff n =
    moveByteArray dst (doff*siz) src (soff*siz) (n*siz)
  where siz = sizeOf (undefined :: a)

-- | Fill a slice of a mutable primitive array with a value.
-- The offset and length are given in elements of type @a@.
setPrimArray :: forall m a. (PrimMonad m, Prim a)
             => MutablePrimArray (PrimState m) a -- ^ array to fill
             -> Int                              -- ^ offset into array
             -> Int                              -- ^ number of values to fill
             -> a                                -- ^ value to fill with
             -> m ()
{-# INLINE setPrimArray #-}
setPrimArray (MutablePrimArray mba) = setByteArray mba

-- | Resize a primitive array using 'resizeMutableByteArray#'.
--
-- To avoid undefined behaviour, the original 'MutablePrimArray' shall not be accessed anymore.
--
resizeMutablePrimArray :: forall m a. (PrimMonad m, Prim a)
                       => MutablePrimArray (PrimState m) a
                       -> Int
                       -> m (MutablePrimArray (PrimState m) a)
resizeMutablePrimArray (MutablePrimArray mba) sz =
    MutablePrimArray `liftM` resizeMutableByteArray mba (sz * siz)
  where siz = sizeOf (undefined :: a)
{-# INLINE resizeMutablePrimArray #-}

-- | Shrink a primitive array using 'shrinkMutableByteArray#'.
--
-- The new size argument must be less than or equal to the current size, but it's not checked.
--
shrinkMutablePrimArray :: forall m a. (Prim a, PrimMonad m)
                       => MutablePrimArray (PrimState m) a
                       -> Int
                       -> m ()
shrinkMutablePrimArray (MutablePrimArray mba) sz =
    shrinkMutableByteArray mba (sz * siz)
  where siz = sizeOf (undefined :: a)
{-# INLINE shrinkMutablePrimArray #-}

-- | Check if the two immutable arrays refer to the same memory block.
samePrimArray :: PrimArray a -> PrimArray a -> Bool
{-# INLINE samePrimArray #-}
samePrimArray (PrimArray (ByteArray ba1#)) (PrimArray (ByteArray ba2#)) =
    isTrue# (sameMutableByteArray# (unsafeCoerce# ba1#) (unsafeCoerce# ba2#))

--------------------------------------------------------------------------------

prefetchPrimArray0, prefetchPrimArray1, prefetchPrimArray2, prefetchPrimArray3
    :: PrimMonad m => PrimArray a -> Int -> m ()
{-# INLINE prefetchPrimArray0 #-}
{-# INLINE prefetchPrimArray1 #-}
{-# INLINE prefetchPrimArray2 #-}
{-# INLINE prefetchPrimArray3 #-}
prefetchPrimArray0 (PrimArray ba) = prefetchByteArray0 ba
prefetchPrimArray1 (PrimArray ba) = prefetchByteArray1 ba
prefetchPrimArray2 (PrimArray ba) = prefetchByteArray2 ba
prefetchPrimArray3 (PrimArray ba) = prefetchByteArray3 ba


prefetchMutablePrimArray0, prefetchMutablePrimArray1, prefetchMutablePrimArray2, prefetchMutablePrimArray3
    :: PrimMonad m => MutablePrimArray (PrimState m) a -> Int -> m ()
{-# INLINE prefetchMutablePrimArray0 #-}
{-# INLINE prefetchMutablePrimArray1 #-}
{-# INLINE prefetchMutablePrimArray2 #-}
{-# INLINE prefetchMutablePrimArray3 #-}
prefetchMutablePrimArray0 (MutablePrimArray mba) = prefetchMutableByteArray0 mba
prefetchMutablePrimArray1 (MutablePrimArray mba) = prefetchMutableByteArray1 mba
prefetchMutablePrimArray2 (MutablePrimArray mba) = prefetchMutableByteArray2 mba
prefetchMutablePrimArray3 (MutablePrimArray mba) = prefetchMutableByteArray3 mba

--------------------------------------------------------------------------------
--
-- | Check if a primitive array is pinned.
--
isPrimArrayPinned :: PrimArray a -> Bool
{-# INLINE isPrimArrayPinned #-}
isPrimArrayPinned (PrimArray ba) = isByteArrayPinned ba

-- | Check if a mutable primitive array is pinned.
--
isMutablePrimArrayPinned :: MutablePrimArray s a -> Bool
{-# INLINE isMutablePrimArrayPinned #-}
isMutablePrimArrayPinned (MutablePrimArray mba) = isMutableByteArrayPinned mba
