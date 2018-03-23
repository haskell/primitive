{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Data.Primitive.Ptr
-- Copyright   : (c) Roman Leshchinskiy 2009-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Primitive operations on machine addresses
--

module Data.Primitive.Ptr (
  -- * Types
  Ptr(..),

  -- * Address arithmetic
  nullPtr, advancePtr,

  -- * Element access
  indexOffPtr, readOffPtr, writeOffPtr,

  -- * Block operations
  copyPtr, movePtr, setPtr
) where

import Control.Monad.Primitive
import Data.Primitive.Types

import GHC.Base ( Int(..) )
import GHC.Prim

import GHC.Ptr
import Foreign.Marshal.Utils


-- | Offset a pointer by the given number of elements.
advancePtr :: forall a. Prim a => Ptr a -> Int -> Ptr a
advancePtr (Ptr a#) (I# i#) = Ptr (plusAddr# a# (i# *# sizeOf# (undefined :: a)))

-- | Read a value from a memory position given by a pointer and an offset.
-- The memory block the address refers to must be immutable. The offset is in
-- elements of type @a@ rather than in bytes.
indexOffPtr :: Prim a => Ptr a -> Int -> a
{-# INLINE indexOffPtr #-}
indexOffPtr (Ptr addr#) (I# i#) = indexOffAddr# addr# i#

-- | Read a value from a memory position given by an address and an offset.
-- The offset is in elements of type @a@ rather than in bytes.
readOffPtr :: (Prim a, PrimMonad m) => Ptr a -> Int -> m a
{-# INLINE readOffPtr #-}
readOffPtr (Ptr addr#) (I# i#) = primitive (readOffAddr# addr# i#)

-- | Write a value to a memory position given by an address and an offset.
-- The offset is in elements of type @a@ rather than in bytes.
writeOffPtr :: (Prim a, PrimMonad m) => Ptr a -> Int -> a -> m ()
{-# INLINE writeOffPtr #-}
writeOffPtr (Ptr addr#) (I# i#) x = primitive_ (writeOffAddr# addr# i# x)

-- | Copy the given number of bytes from the second 'Ptr to the first. The
-- areas may not overlap.
copyPtr :: PrimMonad m
  => Ptr a -- ^ destination pointer
  -> Ptr a -- ^ source pointer
  -> Int -- ^ number of elements
  -> m ()
{-# INLINE copyPtr #-}
copyPtr (Ptr dst#) (Ptr src#) n
  = unsafePrimToPrim $ copyBytes (Ptr dst#) (Ptr src#) n

-- | Copy the given number of bytes from the second 'Ptr to the first. The
-- areas may overlap.
movePtr :: PrimMonad m
  => Ptr a -- ^ destination address
  -> Ptr a -- ^ source address
  -> Int -- ^ number of elements
  -> m ()
{-# INLINE movePtr #-}
movePtr (Ptr dst#) (Ptr src#) n
  = unsafePrimToPrim $ moveBytes (Ptr dst#) (Ptr src#) n

-- | Fill a memory block of with the given value. The length is in
-- elements of type @a@ rather than in bytes.
setPtr :: (Prim a, PrimMonad m) => Ptr a -> Int -> a -> m ()
{-# INLINE setPtr #-}
setPtr (Ptr addr#) (I# n#) x = primitive_ (setOffAddr# addr# 0# n# x)


