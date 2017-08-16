{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Data.Array.Checked
Description : Bounded checked boxed and unboxed arrays
Copyright   : (c) Winter, 2017
License     : BSD3
Maintainer  : libraries@haskell.org, drkoster@qq.com

Stability   : experimental
Portability : non-portable

This module provides exactly the same API with "Data.Primitive.Array.Class", but will throw an 'IndexOutOfBounds'
'ArrayException' on bound check failure.

-}
module Data.Primitive.Array.Checked
  ( -- * Arr typeclass re-export
    A.Arr
  , RealWorld
    -- * Bound checked array operations
  , newArr
  , newArrWith
  , readArr
  , writeArr
  , setArr
  , indexArr
  , indexArrM
  , freezeArr
  , thawArr
  , copyArr
  , copyMutableArr
  , moveArr
  , cloneArr
  , cloneMutableArr
  , resizeMutableArr
  , shrinkMutableArr
  -- * No bound checked operations
  , A.unsafeFreezeArr
  , A.unsafeThawArr
  , A.sameMutableArr
  , A.sizeofArr
  , A.sizeofMutableArr
  , A.sameArr
  -- * Boxed array type
  , A.Array(..)
  , A.MutableArray(..)
  , A.SmallArray(..)
  , A.SmallMutableArray(..)
  , A.uninitialized
  -- * Primitive array type
  , A.PrimArray(..)
  , A.MutablePrimArray(..)
  , A.newPinnedPrimArray, A.newAlignedPinnedPrimArray
  -- * Bound checked primitive array operations
  , copyPrimArrayToPtr, copyMutablePrimArrayToPtr, copyMutablePrimArrayFromPtr
  -- * Unlifted array type
  , A.UnliftedArray(..)
  , A.MutableUnliftedArray(..)
  , A.PrimUnlifted(..)
  -- * The 'ArrayException' type
  , ArrayException(..)
  ) where

import qualified Data.Primitive.Array.Class as A
import Control.Exception (throw, ArrayException(..))
import Control.Monad.Primitive
import Data.Primitive.Types
import GHC.Ptr (Ptr(..))

check :: String -> Bool -> a -> a
check _      True  x = x
check errMsg False _ = throw (IndexOutOfBounds $ "Data.Primitive.Array.Checked." ++ errMsg)
{-# INLINE check #-}

newArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => Int -> m (marr s a)
newArr n = check "newArr: negative size" (n>=0) (A.newArr n)
{-# INLINE newArr #-}

newArrWith :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => Int -> a -> m (marr s a)
newArrWith n x = check "newArrWith: negative size" (n>=0) (A.newArrWith n x)
{-# INLINE newArrWith #-}

readArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m a
readArr marr i = do
    siz <- A.sizeofMutableArr marr
    check "readArr: index of out bounds"
        (i>=0 && i<siz)
        (A.readArr marr i)
{-# INLINE readArr #-}

writeArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => marr s a -> Int -> a -> m ()
writeArr marr i x = do
    siz <- A.sizeofMutableArr marr
    check "writeArr: index of out bounds"
        (i>=0 && i<siz)
        (A.writeArr marr i x)
{-# INLINE writeArr #-}

setArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> a -> m ()
setArr marr s l x = do
    siz <- A.sizeofMutableArr marr
    check "setArr: index range of out bounds"
        (s>=0 && l>=0 && (s+l)<=siz)
        (A.setArr marr s l x)
{-# INLINE setArr #-}

indexArr :: (A.Arr marr arr a) => arr a -> Int -> a
indexArr arr i = check "indexArr: index of out bounds"
    (i>=0 && i<A.sizeofArr arr)
    (A.indexArr arr i)
{-# INLINE indexArr #-}

indexArrM :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => arr a -> Int -> m a
indexArrM arr i = check "indexArrM: index of out bounds"
    (i>=0 && i<A.sizeofArr arr)
    (A.indexArrM arr i)
{-# INLINE indexArrM #-}

freezeArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> m (arr a)
freezeArr marr s l = do
    siz <- A.sizeofMutableArr marr
    check "freezeArr: index range of out bounds"
        (s>=0 && l>=0 && (s+l)<=siz)
        (A.freezeArr marr s l)
{-# INLINE freezeArr #-}

thawArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => arr a -> Int -> Int -> m (marr s a)
thawArr arr s l = check "thawArr: index range of out bounds"
    (s>=0 && l>=0 && (s+l)<=A.sizeofArr arr)
    (A.thawArr arr s l)
{-# INLINE thawArr #-}

copyArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => marr s a -> Int -> arr a -> Int -> Int -> m ()
copyArr marr s1 arr s2 l = do
    siz <- A.sizeofMutableArr marr
    check "copyArr: index range of out bounds"
        (s1>=0 && s2>=0 && l>=0 && (s2+l)<=A.sizeofArr arr && (s1+l)<=siz)
        (A.copyArr marr s1 arr s2 l)
{-# INLINE copyArr #-}

copyMutableArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => marr s a -> Int -> marr s a -> Int -> Int -> m ()
copyMutableArr marr1 s1 marr2 s2 l = do
    siz1 <- A.sizeofMutableArr marr1
    siz2 <- A.sizeofMutableArr marr2
    check "copyMutableArr: index range of out bounds"
        (s1>=0 && s2>=0 && l>=0 && (s2+l)<=siz2 && (s1+l)<=siz1)
        (A.copyMutableArr marr1 s1 marr2 s2 l)
{-# INLINE copyMutableArr #-}

moveArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => marr s a -> Int -> marr s a -> Int -> Int -> m ()
moveArr marr1 s1 marr2 s2 l = do
    siz1 <- A.sizeofMutableArr marr1
    siz2 <- A.sizeofMutableArr marr2
    check "moveArr: index range of out bounds"
        (s1>=0 && s2>=0 && l>=0 && (s2+l)<=siz2 && (s1+l)<=siz1)
        (A.copyMutableArr marr1 s1 marr2 s2 l)
{-# INLINE moveArr #-}

cloneArr :: (A.Arr marr arr a) => arr a -> Int -> Int -> arr a
cloneArr arr s l = check "cloneArr: index range of out bounds"
    (s>=0 && l>=0 && (s+l)<=A.sizeofArr arr)
    (A.cloneArr arr s l)
{-# INLINE cloneArr #-}

cloneMutableArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> m (marr s a)
cloneMutableArr marr s l = do
    siz <- A.sizeofMutableArr marr
    check "cloneMutableArr: index range of out bounds"
        (s>=0 && l>=0 && (s+l)<=siz)
        (A.cloneMutableArr marr s l)
{-# INLINE cloneMutableArr #-}

resizeMutableArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m (marr s a)
resizeMutableArr marr n = check "resizeMutableArr: negative index"
    (n>=0)
    (A.resizeMutableArr marr n)
{-# INLINE resizeMutableArr #-}

-- | New size should be >= 0, and <= original size.
--
shrinkMutableArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m ()
shrinkMutableArr marr n = do
    siz <- A.sizeofMutableArr marr
    check "shrinkMutableArr: size out of bounds"
        (n>=0 && n<=siz)
        (A.shrinkMutableArr marr n)
{-# INLINE shrinkMutableArr #-}

copyPrimArrayToPtr :: (PrimMonad m, Prim a)
                   => Ptr a
                   -> A.PrimArray a
                   -> Int
                   -> Int
                   -> m ()
{-# INLINE copyPrimArrayToPtr #-}
copyPrimArrayToPtr ptr arr s l = check "copyPrimArrayToPtr: index range out of bounds"
    (s>=0 && l>=0 && (s+l)<=A.sizeofArr arr)
    (A.copyPrimArrayToPtr ptr arr s l)

copyMutablePrimArrayToPtr :: (PrimMonad m, Prim a)
                          => Ptr a
                          -> A.MutablePrimArray (PrimState m) a
                          -> Int
                          -> Int
                          -> m ()
{-# INLINE copyMutablePrimArrayToPtr #-}
copyMutablePrimArrayToPtr ptr marr s l = do
    siz <- A.sizeofMutableArr marr
    check "copyMutablePrimArrayToPtr: index range out of bounds"
        (s>=0 && l>=0 && (s+l)<=siz)
        (A.copyMutablePrimArrayToPtr ptr marr s l)

copyMutablePrimArrayFromPtr :: (PrimMonad m, Prim a)
                            => A.MutablePrimArray (PrimState m) a
                            -> Int
                            -> Ptr a
                            -> Int
                            -> m ()
{-# INLINE copyMutablePrimArrayFromPtr #-}
copyMutablePrimArrayFromPtr marr s ptr l = do
    siz <- A.sizeofMutableArr marr
    check "copyMutablePrimArrayFromPtr: index range out of bounds"
        (s>=0 && l>=0 && (s+l)<=siz)
        (A.copyMutablePrimArrayFromPtr marr s ptr l)
