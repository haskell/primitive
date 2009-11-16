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

  newArray, readArray, writeArray, indexArray, indexArrayM,
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
{-# INLINE newArray #-}
newArray (I# n#) x = primitive
   (\s# -> case newArray# n# x s# of
             (# s'#, arr# #) -> (# s'#, MutableArray arr# #))

-- | Read a value from the array at the given index.
readArray :: PrimMonad m => MutableArray m a -> Int -> m a
{-# INLINE readArray #-}
readArray (MutableArray arr#) (I# i#) = primitive (readArray# arr# i#)

-- | Write a value to the array at the given index.
writeArray :: PrimMonad m => MutableArray m a -> Int -> a -> m ()
{-# INLINE writeArray #-}
writeArray (MutableArray arr#) (I# i#) x = primitive_ (writeArray# arr# i# x)

-- | Read a value from the array at the given index.
indexArray :: Array a -> Int -> a
{-# INLINE indexArray #-}
indexArray (Array arr#) (I# i#) = case indexArray# arr# i# of (# x #) -> x

-- | Monadically read a value from the array at the given index.
-- This allows us to be strict in the array while remaining lazy in the read
-- element which is very useful for collective operations. Suppose we want to
-- copy an array. We could do something like this:
--
-- > copy marr arr ... = do ...
-- >                        writeArray marr i (indexArray arr i) ...
-- >                        ...
--
-- But since primitive arrays are lazy, the calls to 'indexArray' will not be
-- evaluated. Rather, @marr@ will be filled with thunks each of which would
-- retain a reference to @arr@. This is definitely not what we want!
--
-- With 'indexArrayM', we can instead write
--
-- > copy marr arr ... = do ...
-- >                        x <- indexArrayM arr i
-- >                        writeArray marr i x
-- >                        ...
--
-- Now, indexing is executed immediately although the returned element is
-- still not evaluated.
--
indexArrayM :: Monad m => Array a -> Int -> m a 
{-# INLINE indexArrayM #-}
indexArrayM (Array arr#) (I# i#)
  = case indexArray# arr# i# of (# x #) -> return x

-- | Convert a mutable array to an immutable one without copying. The
-- array should not be modified after the conversion.
unsafeFreezeArray :: PrimMonad m => MutableArray m a -> m (Array a)
{-# INLINE unsafeFreezeArray #-}
unsafeFreezeArray (MutableArray arr#)
  = primitive (\s# -> case unsafeFreezeArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, Array arr'# #))

-- | Convert an immutable array to an mutable one without copying. The
-- immutable array should not be used after the conversion.
unsafeThawArray :: PrimMonad m => Array a -> m (MutableArray m a)
{-# INLINE unsafeThawArray #-}
unsafeThawArray (Array arr#)
  = primitive (\s# -> case unsafeThawArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, MutableArray arr'# #))

-- | Check whether the two arrays refer to the same memory block.
sameMutableArray :: MutableArray m a -> MutableArray m a -> Bool
{-# INLINE sameMutableArray #-}
sameMutableArray (MutableArray arr#) (MutableArray brr#)
  = sameMutableArray# arr# brr#

