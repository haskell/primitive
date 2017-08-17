{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

{-|
Module      : Data.Array
Description : Unified boxed and unboxed arrays
Copyright   : (c) Winter, 2017
License     : BSD3
Maintainer  : libraries@haskell.org, drkoster@qq.com
Stability   : experimental
Portability : non-portable

Unified unboxed and boxed array operations using functional dependencies.

All operations are NOT bound checked, if you need checked operations please use "Data.Array.Checked".
It exports exactly same APIs so that you can switch between without pain.

-}

module Data.Primitive.Array.Class (
  -- * Arr typeclass
    Arr(..)
  , RealWorld
  -- * Boxed array type
  , Array(..)
  , MutableArray(..)
  , SmallArray(..)
  , SmallMutableArray(..)
  , uninitialized
  -- * Primitive array type
  , PrimArray(..)
  , MutablePrimArray(..)
  , Prim(..)
  , newPinnedPrimArray, newAlignedPinnedPrimArray
  , copyPrimArrayToPtr, copyMutablePrimArrayToPtr, copyMutablePrimArrayFromPtr
  -- * Unlifted array type
  , UnliftedArray(..)
  , MutableUnliftedArray(..)
  , PrimUnlifted(..)
  -- * The 'ArrayException' type
  , ArrayException(..)
  ) where

import Data.Primitive.Types
import Control.Monad.Primitive
import Control.Exception (ArrayException(..), throw)
import Data.Primitive.PrimArray
import Data.Primitive.Array
import Data.Primitive.SmallArray
import Data.Primitive.UnliftedArray
import GHC.ST
import GHC.Prim
import Data.Primitive.Internal.Compat ( isTrue# )

#if (__GLASGOW_HASKELL__ >= 710)
#define HAVE_SMALL_ARRAY 1
#endif

-- | Bottom value (@throw ('UndefinedElement' "Data.Array.uninitialized")@)
-- for initialize new boxed array('Array', 'SmallArray'..).
--
uninitialized :: a
uninitialized = throw (UndefinedElement "Data.Primitive.Array.Class.uninitialized")

-- | A typeclass to unify box & unboxed, mutable & immutable array operations.
--
-- Most of these functions simply wrap their primitive counterpart. If there are no primitive ones,
-- the method is emulated using other operations to get the same semantics.
--
class Arr (marr :: * -> * -> *) (arr :: * -> * ) a | arr -> marr, marr -> arr where

    -- | Make a new array with given size.
    --
    -- It's not safe to access uninitialized element.
    -- For boxed arrays, all elements are @uninitialized@, a thunk that throws
    -- an error if it is forced. For primitive array, elements are uninitialized memory.
    -- For unlifted array , elements are the reference to itself.
    --
    newArr :: (PrimMonad m, PrimState m ~ s) => Int -> m (marr s a)

    -- | Make a new array and fill it with an initial value.
    newArrWith :: (PrimMonad m, PrimState m ~ s) => Int -> a -> m (marr s a)

    -- | Index a mutable array in a primitive monad.
    readArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m a

    -- | Write a mutable array in a primitive monad.
    writeArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> a -> m ()

    -- | Fill a mutable array with a given value.
    setArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> a -> m ()

    -- | Index an immutable array, which is a pure operation.
    indexArr :: arr a -> Int -> a

    -- | Index an immutable array in a primitive monad. This helps in situations where
    -- you want your indexing result to not be a thunk referencing whole array.
    indexArrM :: (Monad m) => arr a -> Int -> m a

    -- | Safely freeze a mutable array by making an immutable copy of its slice.
    freezeArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> m (arr a)

    -- | Safely thaw an immutable array by making a mutable copy of its slice.
    thawArr :: (PrimMonad m, PrimState m ~ s) => arr a -> Int -> Int -> m (marr s a)

    -- | In-place freeze a mutable array. The original mutable array can not be used
    -- anymore.
    unsafeFreezeArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> m (arr a)

    -- | In-place thaw an immutable array. The original immutable array can not be used
    -- anymore.
    unsafeThawArr :: (PrimMonad m, PrimState m ~ s) => arr a -> m (marr s a)

    -- | Copy a slice of an immutable array to a mutable array at the given offset.
    copyArr ::  (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> arr a -> Int -> Int -> m ()

    -- | Copy a slice of a mutable array to another mutable array at given offset.
    -- The two mutable arrays must not be the same one. The first array is
    -- the destination and the second is the source.
    copyMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> marr s a -> Int -> Int -> m ()

    -- | Copy a slice of a mutable array to another mutable array at the given offset.
    -- The two mutable arrays are allowed to be the same one.
    moveArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> marr s a -> Int -> Int -> m ()

    -- | Create an immutable copy.
    cloneArr :: arr a -> Int -> Int -> arr a

    -- | Create a mutable copy.
    cloneMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> m (marr s a)

    -- | Resize a mutable array to the given size.
    resizeMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m (marr s a)

    -- | Shrink a mutable array to the given size.
    --
    -- This operation is not guaranteed to have effects, e.g. 'sizeOfMutableArr' may not change.
    -- It's mainly used to optimize GC of arrays which contain unused elements.
    shrinkMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m ()

    -- | Check to see if the two mutable arrays refer to the same memory.
    sameMutableArr :: marr s a -> marr s a -> Bool

    -- | Size of an immutable array.
    sizeofArr :: arr a -> Int

    -- | Size of a mutable array.
    sizeofMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> m Int

    -- | Check to see if the two arrays refer to the same memory.
    --
    -- Note that the result of 'sameArr' may change depending on GHC\'s optimizations.
    -- For example
    -- @let arr = runST ... in arr 'sameArr' arr@ may return false if compiler decides to
    -- inline it.
    --
    -- See https://ghc.haskell.org/trac/ghc/ticket/13908 for more background.
    --
    sameArr :: arr a -> arr a -> Bool

instance Arr MutableArray Array a where
    newArr n = newArray n uninitialized
    {-# INLINE newArr #-}
    newArrWith = newArray
    {-# INLINE newArrWith #-}
    readArr = readArray
    {-# INLINE readArr #-}
    writeArr = writeArray
    {-# INLINE writeArr #-}
    setArr marr s l x = go s
      where
        !sl = s + l
        go !i | i >= sl = return ()
              | otherwise = writeArray marr i x >> go (i+1)
    {-# INLINE setArr #-}
    indexArr = indexArray
    {-# INLINE indexArr #-}
    indexArrM = indexArrayM
    {-# INLINE indexArrM #-}
    freezeArr = freezeArray
    {-# INLINE freezeArr #-}
    thawArr = thawArray
    {-# INLINE thawArr #-}
    unsafeFreezeArr = unsafeFreezeArray
    {-# INLINE unsafeFreezeArr #-}
    unsafeThawArr = unsafeThawArray
    {-# INLINE unsafeThawArr #-}

    copyArr = copyArray
    {-# INLINE copyArr #-}
    copyMutableArr = copyMutableArray
    {-# INLINE copyMutableArr #-}

    moveArr marr1 s1 marr2 s2 l
        | l <= 0 = return ()
        | sameMutableArray marr1 marr2 =
            case compare s1 s2 of
                LT ->
                    let !d = s2 - s1
                        !s2l = s2 + l
                        go !i | i >= s2l = return ()
                              | otherwise = do x <- readArray marr2 i
                                               writeArray marr1 (i-d) x
                                               go (i+1)
                    in go s2

                EQ -> return ()

                GT ->
                    let !d = s1 - s2
                        go !i | i < s2 = return ()
                              | otherwise = do x <- readArray marr2 i
                                               writeArray marr1 (i+d) x
                                               go (i-1)
                    in go (s2+l-1)
        | otherwise = copyMutableArray marr1 s1 marr2 s2 l
    {-# INLINE moveArr #-}

    cloneArr = cloneArray
    {-# INLINE cloneArr #-}
    cloneMutableArr = cloneMutableArray
    {-# INLINE cloneMutableArr #-}

    resizeMutableArr marr n = do
        marr' <- newArray n uninitialized
        copyMutableArray marr' 0 marr 0 (sizeofMutableArray marr)
        return marr'
    {-# INLINE resizeMutableArr #-}
    shrinkMutableArr _ _ = return ()
    {-# INLINE shrinkMutableArr #-}

    sameMutableArr = sameMutableArray
    {-# INLINE sameMutableArr #-}
    sizeofArr = sizeofArray
    {-# INLINE sizeofArr #-}
    sizeofMutableArr = return . sizeofMutableArray
    {-# INLINE sizeofMutableArr #-}

    sameArr (Array arr1#) (Array arr2#) = isTrue# (
        sameMutableArray# (unsafeCoerce# arr1#) (unsafeCoerce# arr2#))
    {-# INLINE sameArr #-}

instance Arr SmallMutableArray SmallArray a where
    newArr n = newSmallArray n uninitialized
    {-# INLINE newArr #-}
    newArrWith = newSmallArray
    {-# INLINE newArrWith #-}
    readArr = readSmallArray
    {-# INLINE readArr #-}
    writeArr = writeSmallArray
    {-# INLINE writeArr #-}
    setArr marr s l x = go s
      where
        !sl = s + l
        go !i | i >= sl = return ()
              | otherwise = writeSmallArray marr i x >> go (i+1)
    {-# INLINE setArr #-}
    indexArr = indexSmallArray
    {-# INLINE indexArr #-}
    indexArrM = indexSmallArrayM
    {-# INLINE indexArrM #-}
    freezeArr = freezeSmallArray
    {-# INLINE freezeArr #-}
    thawArr = thawSmallArray
    {-# INLINE thawArr #-}
    unsafeFreezeArr = unsafeFreezeSmallArray
    {-# INLINE unsafeFreezeArr #-}
    unsafeThawArr = unsafeThawSmallArray
    {-# INLINE unsafeThawArr #-}

    copyArr = copySmallArray
    {-# INLINE copyArr #-}
    copyMutableArr = copySmallMutableArray
    {-# INLINE copyMutableArr #-}

    moveArr marr1 s1 marr2 s2 l
        | l <= 0 = return ()
        | sameMutableArr marr1 marr2 =
            case compare s1 s2 of
                LT ->
                    let !d = s2 - s1
                        !s2l = s2 + l
                        go !i | i >= s2l = return ()
                              | otherwise = do x <- readSmallArray marr2 i
                                               writeSmallArray marr1 (i-d) x
                                               go (i+1)
                    in go s2

                EQ -> return ()

                GT ->
                    let !d = s1 - s2
                        go !i | i < s2 = return ()
                              | otherwise = do x <- readSmallArray marr2 i
                                               writeSmallArray marr1 (i+d) x
                                               go (i-1)
                    in go (s2+l-1)
        | otherwise = copySmallMutableArray marr1 s1 marr2 s2 l
    {-# INLINE moveArr #-}

    cloneArr = cloneSmallArray
    {-# INLINE cloneArr #-}
    cloneMutableArr = cloneSmallMutableArray
    {-# INLINE cloneMutableArr #-}

    resizeMutableArr marr n = do
        marr' <- newSmallArray n uninitialized
        copySmallMutableArray marr' 0 marr 0 (sizeofSmallMutableArray marr)
        return marr'
    {-# INLINE resizeMutableArr #-}
    shrinkMutableArr _ _ = return ()
    {-# INLINE shrinkMutableArr #-}

    sameMutableArr (SmallMutableArray smarr1#) (SmallMutableArray smarr2#) = isTrue#
#if HAVE_SMALL_ARRAY
        (sameSmallMutableArray# smarr1# smarr2#)
#else
        (sameMutableArray# smarr1# smarr2#)
#endif
    {-# INLINE sameMutableArr #-}

    sizeofArr = sizeofSmallArray
    {-# INLINE sizeofArr #-}
    sizeofMutableArr = return . sizeofSmallMutableArray
    {-# INLINE sizeofMutableArr #-}

    sameArr (SmallArray arr1#) (SmallArray arr2#) = isTrue#
#if HAVE_SMALL_ARRAY
        (sameSmallMutableArray# (unsafeCoerce# arr1#) (unsafeCoerce# arr2#))
#else
        (sameMutableArray# (unsafeCoerce# arr1#) (unsafeCoerce# arr2#))
#endif
    {-# INLINE sameArr #-}

instance Prim a => Arr MutablePrimArray PrimArray a where
    newArr = newPrimArray
    {-# INLINE newArr #-}
    newArrWith n x = do
        marr <- newPrimArray n
        setPrimArray marr 0 n x
        return marr
    {-# INLINE newArrWith #-}
    readArr = readPrimArray
    {-# INLINE readArr #-}
    writeArr = writePrimArray
    {-# INLINE writeArr #-}
    setArr = setPrimArray
    {-# INLINE setArr #-}
    indexArr = indexPrimArray
    {-# INLINE indexArr #-}
    indexArrM arr i = return (indexPrimArray arr i)
    {-# INLINE indexArrM #-}
    freezeArr marr s l = do
        marr' <- newPrimArray l
        copyMutablePrimArray marr' 0 marr s l
        unsafeFreezePrimArray marr'
    {-# INLINE freezeArr #-}
    thawArr arr s l = do
        marr' <- newPrimArray l
        copyPrimArray marr' 0 arr s l
        return marr'
    {-# INLINE thawArr #-}
    unsafeFreezeArr = unsafeFreezePrimArray
    {-# INLINE unsafeFreezeArr #-}
    unsafeThawArr = unsafeThawPrimArray
    {-# INLINE unsafeThawArr #-}

    copyArr = copyPrimArray
    {-# INLINE copyArr #-}
    copyMutableArr = copyMutablePrimArray
    {-# INLINE copyMutableArr #-}

    moveArr = movePrimArray
    {-# INLINE moveArr #-}

    cloneArr arr s l = runST (do
            marr <- newPrimArray l
            copyPrimArray marr 0 arr s l
            unsafeFreezePrimArray marr
        )
    {-# INLINE cloneArr #-}
    cloneMutableArr marr s l = do
        marr' <- newPrimArray l
        copyMutablePrimArray marr' 0 marr s l
        return marr'
    {-# INLINE cloneMutableArr #-}

    resizeMutableArr = resizeMutablePrimArray
    {-# INLINE resizeMutableArr #-}
    shrinkMutableArr = shrinkMutablePrimArray
    {-# INLINE shrinkMutableArr #-}

    sameMutableArr = sameMutablePrimArray
    {-# INLINE sameMutableArr #-}
    sizeofArr = sizeofPrimArray
    {-# INLINE sizeofArr #-}
    sizeofMutableArr = sizeofMutablePrimArray
    {-# INLINE sizeofMutableArr #-}

    sameArr = samePrimArray
    {-# INLINE sameArr #-}

instance PrimUnlifted a => Arr MutableUnliftedArray UnliftedArray a where
    newArr = unsafeNewUnliftedArray
    {-# INLINE newArr #-}
    newArrWith = newUnliftedArray
    {-# INLINE newArrWith #-}
    readArr = readUnliftedArray
    {-# INLINE readArr #-}
    writeArr = writeUnliftedArray
    {-# INLINE writeArr #-}
    setArr marr s l x = go s
      where
        !sl = s + l
        go !i | i >= sl = return ()
              | otherwise = writeUnliftedArray marr i x >> go (i+1)
    {-# INLINE setArr #-}
    indexArr = indexUnliftedArray
    {-# INLINE indexArr #-}
    indexArrM = indexUnliftedArrayM
    {-# INLINE indexArrM #-}
    freezeArr = freezeUnliftedArray
    {-# INLINE freezeArr #-}
    thawArr = thawUnliftedArray
    {-# INLINE thawArr #-}
    unsafeFreezeArr = unsafeFreezeUnliftedArray
    {-# INLINE unsafeFreezeArr #-}
    unsafeThawArr (UnliftedArray arr#) = primitive ( \ s0# ->
            let !(# s1#, marr# #) = unsafeThawArray# (unsafeCoerce# arr#) s0#   -- ArrayArray# and Array# use the same representation
            in (# s1#, MutableUnliftedArray (unsafeCoerce# marr#) #)            -- so this works
        )
    {-# INLINE unsafeThawArr #-}

    copyArr = copyUnliftedArray
    {-# INLINE copyArr #-}
    copyMutableArr = copyMutableUnliftedArray
    {-# INLINE copyMutableArr #-}

    moveArr marr1 s1 marr2 s2 l
        | l <= 0 = return ()
        | sameMutableUnliftedArray marr1 marr2 =
            case compare s1 s2 of
                LT ->
                    let !d = s2 - s1
                        !s2l = s2 + l
                        go !i | i >= s2l = return ()
                              | otherwise = do x <- readUnliftedArray marr2 i
                                               writeUnliftedArray marr1 (i-d) x
                                               go (i+1)
                    in go s2

                EQ -> return ()

                GT ->
                    let !d = s1 - s2
                        go !i | i < s2 = return ()
                              | otherwise = do x <- readUnliftedArray marr2 i
                                               writeUnliftedArray marr1 (i+d) x
                                               go (i-1)
                    in go (s2+l-1)
        | otherwise = copyMutableUnliftedArray marr1 s1 marr2 s2 l
    {-# INLINE moveArr #-}

    cloneArr = cloneUnliftedArray
    {-# INLINE cloneArr #-}
    cloneMutableArr = cloneMutableUnliftedArray
    {-# INLINE cloneMutableArr #-}

    resizeMutableArr marr n = do
        marr' <- newUnliftedArray n uninitialized
        copyMutableUnliftedArray marr' 0 marr 0 (sizeofMutableUnliftedArray marr)
        return marr'
    {-# INLINE resizeMutableArr #-}
    shrinkMutableArr _ _ = return ()
    {-# INLINE shrinkMutableArr #-}

    sameMutableArr = sameMutableUnliftedArray
    {-# INLINE sameMutableArr #-}
    sizeofArr = sizeofUnliftedArray
    {-# INLINE sizeofArr #-}
    sizeofMutableArr = return . sizeofMutableUnliftedArray
    {-# INLINE sizeofMutableArr #-}

    sameArr (UnliftedArray arr1#) (UnliftedArray arr2#) = isTrue# (
        sameMutableArrayArray# (unsafeCoerce# arr1#) (unsafeCoerce# arr2#))
    {-# INLINE sameArr #-}
