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
Description : Fast boxed and unboxed arrays
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
import GHC.Types (isTrue#)

-- | Bottom value (@throw ('UndefinedElement' "Data.Array.uninitialized")@)
-- for initialize new boxed array('Array', 'SmallArray'..).
--
uninitialized :: a
uninitialized = throw (UndefinedElement "Data.Array.uninitialized")

-- | A typeclass to unify box & unboxed, mutable & immutable array operations.
--
-- Most of these functions simply wrap their primitive counterpart, if there's no primitive ones,
-- we polyfilled using other operations to get the same semantics.
--
-- One exception is that 'shrinkMutableArr' only perform closure resizing on 'PrimArray' because
-- current RTS support only that, 'shrinkMutableArr' will do nothing on other array type.
--
-- It's reasonable to trust GHC with specializing & inlining these polymorphric functions.
-- They are used across this package and perform identical to their monomophric counterpart.
--
class Arr (marr :: * -> * -> *) (arr :: * -> * ) a | arr -> marr, marr -> arr where

    -- | Make a new array with given size.
    --
    -- For boxed array, all elements are 'uninitialized' which shall not be accessed.
    -- For primitive array, elements are just random garbage.
    newArr :: (PrimMonad m, PrimState m ~ s) => Int -> m (marr s a)

    -- | Make a new array and fill it with an initial value.
    newArrWith :: (PrimMonad m, PrimState m ~ s) => Int -> a -> m (marr s a)

    -- | Index mutable array in a primitive monad.
    readArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m a

    -- | Write mutable array in a primitive monad.
    writeArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> a -> m ()

    -- | Fill mutable array with a given value.
    setArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> a -> m ()

    -- | Index immutable array, which is a pure operation,
    indexArr :: arr a -> Int -> a

    -- | Index immutable array in a primitive monad, this helps in situations that
    -- you want your indexing result is not a thunk referencing whole array.
    indexArrM :: (Monad m) => arr a -> Int -> m a

    -- | Safely freeze mutable array by make a immutable copy of its slice.
    freezeArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> m (arr a)

    -- | Safely thaw immutable array by make a mutable copy of its slice.
    thawArr :: (PrimMonad m, PrimState m ~ s) => arr a -> Int -> Int -> m (marr s a)

    -- | In place freeze a mutable array, the original mutable array can not be used
    -- anymore.
    unsafeFreezeArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> m (arr a)

    -- | In place thaw a immutable array, the original immutable array can not be used
    -- anymore.
    unsafeThawArr :: (PrimMonad m, PrimState m ~ s) => arr a -> m (marr s a)

    -- | Copy a slice of immutable array to mutable array at given offset.
    copyArr ::  (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> arr a -> Int -> Int -> m ()

    -- | Copy a slice of mutable array to mutable array at given offset.
    -- The two mutable arrays shall no be the same one.
    copyMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> marr s a -> Int -> Int -> m ()

    -- | Copy a slice of mutable array to mutable array at given offset.
    -- The two mutable arrays may be the same one.
    moveArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> marr s a -> Int -> Int -> m ()

    -- | Create immutable copy.
    cloneArr :: arr a -> Int -> Int -> arr a

    -- | Create mutable copy.
    cloneMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> m (marr s a)

    -- | Resize mutable array to given size.
    resizeMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m (marr s a)

    -- | Shrink mutable array to given size. This operation only works on primitive arrays.
    -- For boxed array, this is a no-op, e.g. 'sizeOfMutableArr' will not change.
    shrinkMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m ()

    -- | Is two mutable array are reference equal.
    sameMutableArr :: marr s a -> marr s a -> Bool

    -- | Size of immutable array.
    sizeofArr :: arr a -> Int

    -- | Size of mutable array.
    sizeofMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> m Int

    -- | Is two immutable array are referencing the same one.
    --
    -- Note that 'sameArr' 's result may change depending on compiler's optimizations, for example
    -- @let arr = runST ... in arr `sameArr` arr@ may return false if compiler decides to
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

    sameMutableArr (SmallMutableArray smarr1#) (SmallMutableArray smarr2#) =
        isTrue# (sameSmallMutableArray# smarr1# smarr2#)
    {-# INLINE sameMutableArr #-}
    sizeofArr = sizeofSmallArray
    {-# INLINE sizeofArr #-}
    sizeofMutableArr = return . sizeofSmallMutableArray
    {-# INLINE sizeofMutableArr #-}

    sameArr (SmallArray arr1#) (SmallArray arr2#) = isTrue# (
        sameSmallMutableArray# (unsafeCoerce# arr1#) (unsafeCoerce# arr2#))
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
            let (# s1#, marr# #) = unsafeThawArray# (unsafeCoerce# arr#) s0#  -- ArrayArray# and Array# use the same representation
            in (# s1#, MutableUnliftedArray (unsafeCoerce# marr#) #)          -- so this works
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
