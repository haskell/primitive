{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Unsafe #-}

-- | Note: Edward Kmett wrote everything in this module. 
module Data.Primitive.PrimVar
  ( 
  -- * Primitive References
    PrimVar(..)
  , newPrimVar
  , newPinnedPrimVar
  , newAlignedPinnedPrimVar
  , readPrimVar
  , writePrimVar
  , primVarContents
  , primVarToMutablePrimArray
  , casInt
  , fetchAddInt
  , fetchSubInt
  , fetchAndInt
  , fetchNandInt
  , fetchOrInt
  , fetchXorInt
  , atomicReadInt
  , atomicWriteInt
  ) where

import Control.Monad.Primitive
import Data.Primitive
import GHC.Exts
import GHC.Ptr (castPtr)

--------------------------------------------------------------------------------
-- * Primitive References
--------------------------------------------------------------------------------

newtype PrimVar s a = PrimVar (MutablePrimArray s a)

type role PrimVar nominal nominal

-- | Create a primitive reference.
newPrimVar :: (PrimMonad m, Prim a) => a -> m (PrimVar (PrimState m) a)
newPrimVar a = do
  m <- newPrimArray 1
  writePrimArray m 0 a
  return (PrimVar m)
{-# INLINE newPrimVar #-}

-- | Create a pinned primitive reference.
newPinnedPrimVar :: (PrimMonad m, Prim a) => a -> m (PrimVar (PrimState m) a)
newPinnedPrimVar a = do
  m <- newPinnedPrimArray 1
  writePrimArray m 0 a
  return (PrimVar m)
{-# INLINE newPinnedPrimVar #-}

-- | Create a pinned primitive reference with the appropriate alignment for its contents.
newAlignedPinnedPrimVar :: (PrimMonad m, Prim a) => a -> m (PrimVar (PrimState m) a)
newAlignedPinnedPrimVar a = do
  m <- newAlignedPinnedPrimArray 1
  writePrimArray m 0 a
  return (PrimVar m)
{-# INLINE newAlignedPinnedPrimVar #-}

-- | Read a primitive value from the reference
readPrimVar :: (PrimMonad m, Prim a) => PrimVar (PrimState m) a -> m a
readPrimVar (PrimVar m) = readPrimArray m 0
{-# INLINE readPrimVar #-}

-- | Write a primitive value to the reference
writePrimVar :: (PrimMonad m, Prim a) => PrimVar (PrimState m) a -> a -> m ()
writePrimVar (PrimVar m) a = writePrimArray m 0 a
{-# INLINE writePrimVar #-}

instance Eq (PrimVar s a) where
  PrimVar m == PrimVar n = sameMutablePrimArray m n
  {-# INLINE (==) #-}

-- | Yield a pointer to the data of a 'PrimVar'. This operation is only safe on pinned byte arrays allocated by
-- 'newPinnedPrimVar' or 'newAlignedPinnedPrimVar'.
primVarContents :: PrimVar s a -> Ptr a
primVarContents (PrimVar m) = castPtr $ mutablePrimArrayContents m
{-# INLINE primVarContents #-}

primVarToMutablePrimArray :: PrimVar s a -> MutablePrimArray s a
primVarToMutablePrimArray (PrimVar m) = m
{-# INLINE primVarToMutablePrimArray #-}

--------------------------------------------------------------------------------
-- * Atomic Operations
--------------------------------------------------------------------------------

-- | Given a primitive reference, the expected old value, and the new value, perform an atomic compare and swap i.e. write the new value if the current value matches the provided old value. Returns the value of the element before the operation. Implies a full memory barrier.
casInt :: PrimMonad m => PrimVar (PrimState m) Int -> Int -> Int -> m Int
casInt (PrimVar (MutablePrimArray m)) (I# old) (I# new) = primitive $ \s -> case casIntArray# m 0# old new s of
  (# s', result #) -> (# s', I# result #)
{-# INLINE casInt #-}

-- | Given a reference, and a value to add, atomically add the value to the element. Returns the value of the element before the operation. Implies a full memory barrier.
fetchAddInt :: PrimMonad m => PrimVar (PrimState m) Int -> Int -> m Int
fetchAddInt (PrimVar (MutablePrimArray m)) (I# x) = primitive $ \s -> case fetchAddIntArray# m 0# x s of
  (# s', result #) -> (# s', I# result #)
{-# INLINE fetchAddInt #-}

-- | Given a reference, and a value to subtract, atomically subtract the value from the element. Returns the value of the element before the operation. Implies a full memory barrier.
fetchSubInt :: PrimMonad m => PrimVar (PrimState m) Int -> Int -> m Int
fetchSubInt (PrimVar (MutablePrimArray m)) (I# x) = primitive $ \s -> case fetchSubIntArray# m 0# x s of
  (# s', result #) -> (# s', I# result #)
{-# INLINE fetchSubInt #-}

-- | Given a reference, and a value to bitwise and, atomically and the value with the element. Returns the value of the element before the operation. Implies a full memory barrier.
fetchAndInt :: PrimMonad m => PrimVar (PrimState m) Int -> Int -> m Int
fetchAndInt (PrimVar (MutablePrimArray m)) (I# x) = primitive $ \s -> case fetchAndIntArray# m 0# x s of
  (# s', result #) -> (# s', I# result #)
{-# INLINE fetchAndInt #-}

-- | Given a reference, and a value to bitwise nand, atomically nand the value with the element. Returns the value of the element before the operation. Implies a full memory barrier.
fetchNandInt :: PrimMonad m => PrimVar (PrimState m) Int -> Int -> m Int
fetchNandInt (PrimVar (MutablePrimArray m)) (I# x) = primitive $ \s -> case fetchNandIntArray# m 0# x s of
  (# s', result #) -> (# s', I# result #)
{-# INLINE fetchNandInt #-}

-- | Given a reference, and a value to bitwise or, atomically or the value with the element. Returns the value of the element before the operation. Implies a full memory barrier.
fetchOrInt :: PrimMonad m => PrimVar (PrimState m) Int -> Int -> m Int
fetchOrInt (PrimVar (MutablePrimArray m)) (I# x) = primitive $ \s -> case fetchOrIntArray# m 0# x s of
  (# s', result #) -> (# s', I# result #)
{-# INLINE fetchOrInt #-}

-- | Given a reference, and a value to bitwise xor, atomically xor the value with the element. Returns the value of the element before the operation. Implies a full memory barrier.
fetchXorInt :: PrimMonad m => PrimVar (PrimState m) Int -> Int -> m Int
fetchXorInt (PrimVar (MutablePrimArray m)) (I# x) = primitive $ \s -> case fetchXorIntArray# m 0# x s of
  (# s', result #) -> (# s', I# result #)
{-# INLINE fetchXorInt #-}

-- | Given a reference, read an element. Implies a full memory barrier.
atomicReadInt :: PrimMonad m => PrimVar (PrimState m) Int -> m Int
atomicReadInt (PrimVar (MutablePrimArray m)) = primitive $ \s -> case atomicReadIntArray# m 0# s of
  (# s', result #) -> (# s', I# result #)
{-# INLINE atomicReadInt #-}

-- | Given a reference, write an element. Implies a full memory barrier.
atomicWriteInt :: PrimMonad m => PrimVar (PrimState m) Int -> Int -> m ()
atomicWriteInt (PrimVar (MutablePrimArray m)) (I# x) = primitive_ $ \s -> atomicWriteIntArray# m 0# x s
{-# INLINE atomicWriteInt #-}
