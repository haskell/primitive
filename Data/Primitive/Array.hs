{-# LANGUAGE MagicHash, UnboxedTuples #-}

-- |
-- Module      : Data.Primitive.Array
-- Copyright   : (c) Roman Leshchinskiy 2009
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
-- 
-- Primitive boxed arrays
--

module Data.Primitive.Array (
  Array(..), MutableArray(..),

  newArray, readArray, writeArray, indexArray,
  unsafeFreezeArray, unsafeThawArray, sameMutableArray
) where

import Control.Monad.Primitive

import GHC.Base  ( Int(..) )
import GHC.Prim

-- | Boxed arrays
data Array a = Array (Array# a)

-- | Mutable boxed arrays associated with a primitive state-transformer monad.
data MutableArray m a = MutableArray (MutableArray# (PrimState m) a)

-- | Create a new mutable array of the specified size and initialise all
-- elements with the given value.
newArray :: PrimMonad m => Int -> a -> m (MutableArray m a)
newArray (I# n#) x = primitive
   (\s# -> case newArray# n# x s# of
             (# s'#, arr# #) -> (# s'#, MutableArray arr# #))

-- | Read a value from the array at the given index.
readArray :: PrimMonad m => MutableArray m a -> Int -> m a
readArray (MutableArray arr#) (I# i#) = primitive (readArray# arr# i#)

-- | Write a value to the array at the given index.
writeArray :: PrimMonad m => MutableArray m a -> Int -> a -> m ()
writeArray (MutableArray arr#) (I# i#) x = primitive_ (writeArray# arr# i# x)

-- | Read a value to the array at the given index and apply a function to it.
-- This allows us to be strict in the vector if we want. Suppose we had
--
-- > leakyIndex :: Array a -> Int -> a
--
-- instead. Now, if we wanted to copy an array, we'd do something like
--
-- > copy marr arr ... = ... writeArray marr i (leakyIndex arr i) ...
--
-- Since primitive arrays are lazy, the call to 'indexArray' would not be
-- evaluated. Rather, @marr@ would be filled with thunks each of which would
-- retain a reference to @arr@. This is definitely not what we want!
--
-- With the current interface, we can instead write
--
-- > copy marr arr ... = ... indexArray arr i (writeArray marr i) ...
--
-- This code does not have the above problem because indexing (but not the
-- returned element!) is evaluated immediately. To get the leaky behaviour of
-- @leakyIndex@, simply use @'indexArray' arr i id@.
--
indexArray :: Array a -> Int -> (a -> b) -> b
indexArray (Array arr#) (I# i#) f = case indexArray# arr# i# of (# x #) -> f x

-- | Convert a mutable array to an immutable one without copying. The
-- array should not be modified after the conversion.
unsafeFreezeArray :: PrimMonad m => MutableArray m a -> m (Array a)
unsafeFreezeArray (MutableArray arr#)
  = primitive (\s# -> case unsafeFreezeArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, Array arr'# #))

-- | Convert an immutable array to an mutable one without copying. The
-- immutable array should not be used after the conversion.
unsafeThawArray :: PrimMonad m => Array a -> m (MutableArray m a)
unsafeThawArray (Array arr#)
  = primitive (\s# -> case unsafeThawArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, MutableArray arr'# #))

-- | Check whether the two arrays refer to the same memory block.
sameMutableArray :: MutableArray m a -> MutableArray m a -> Bool
sameMutableArray (MutableArray arr#) (MutableArray brr#)
  = sameMutableArray# arr# brr#

