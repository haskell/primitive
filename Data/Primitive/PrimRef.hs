{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Unsafe #-}

-- | Note: Edward Kmett wrote everything in this module. 
module Data.Primitive.PrimRef
  ( 
  -- * Primitive References
    PrimRef(..)
  , newPrimRef
  , newPinnedPrimRef
  , newAlignedPinnedPrimRef
  , readPrimRef
  , writePrimRef
  , primRefContents
  -- * Frozen Primitive References
  , FrozenPrimRef(..)
  , newFrozenPrimRef
  , unsafeFreezePrimRef
  , unsafeThawPrimRef
  , indexFrozenPrimRef
  , frozenPrimRefContents
  -- * Atomic Operations
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
import Control.Monad.ST
import Data.Data
import Data.Primitive
import GHC.Exts
import GHC.Ptr (castPtr)

--------------------------------------------------------------------------------
-- * Primitive References
--------------------------------------------------------------------------------

newtype PrimRef s a = PrimRef (MutableByteArray s)

type role PrimRef nominal nominal

-- | Create a primitive reference.
newPrimRef :: (PrimMonad m, Prim a) => a -> m (PrimRef (PrimState m) a)
newPrimRef a = do
  m <- newByteArray (sizeOf a)
  writeByteArray m 0 a
  return (PrimRef m)
{-# INLINE newPrimRef #-}

-- | Create a pinned primitive reference.
newPinnedPrimRef :: (PrimMonad m, Prim a) => a -> m (PrimRef (PrimState m) a)
newPinnedPrimRef a = do
  m <- newPinnedByteArray (sizeOf a)
  writeByteArray m 0 a
  return (PrimRef m)
{-# INLINE newPinnedPrimRef #-}

-- | Create a pinned primitive reference with the appropriate alignment for its contents.
newAlignedPinnedPrimRef :: (PrimMonad m, Prim a) => a -> m (PrimRef (PrimState m) a)
newAlignedPinnedPrimRef a = do
  m <- newAlignedPinnedByteArray (sizeOf a) (alignment a)
  writeByteArray m 0 a
  return (PrimRef m)
{-# INLINE newAlignedPinnedPrimRef #-}

-- | Read a primitive value from the reference
readPrimRef :: (PrimMonad m, Prim a) => PrimRef (PrimState m) a -> m a
readPrimRef (PrimRef m) = readByteArray m 0
{-# INLINE readPrimRef #-}

-- | Write a primitive value to the reference
writePrimRef :: (PrimMonad m, Prim a) => PrimRef (PrimState m) a -> a -> m ()
writePrimRef (PrimRef m) a = writeByteArray m 0 a
{-# INLINE writePrimRef #-}

instance Eq (PrimRef s a) where
  PrimRef m == PrimRef n = sameMutableByteArray m n
  {-# INLINE (==) #-}

-- | Yield a pointer to the data of a 'PrimRef'. This operation is only safe on pinned byte arrays allocated by
-- 'newPinnedPrimRef' or 'newAlignedPinnedPrimRef'.
primRefContents :: PrimRef s a -> Ptr a
primRefContents (PrimRef m) = castPtr $ mutableByteArrayContents m
{-# INLINE primRefContents #-}

--------------------------------------------------------------------------------
-- * Frozen Primitive References
--------------------------------------------------------------------------------

-- | Convert a mutable 'PrimRef' to an immutable one without copying. The reference should not be modified after the conversion.
unsafeFreezePrimRef :: PrimMonad m => PrimRef (PrimState m) a -> m (FrozenPrimRef a)
unsafeFreezePrimRef (PrimRef m) = FrozenPrimRef <$> unsafeFreezeByteArray m
{-# INLINE unsafeFreezePrimRef #-}

newtype FrozenPrimRef a = FrozenPrimRef ByteArray

type role FrozenPrimRef nominal

newFrozenPrimRef :: Prim a => a -> FrozenPrimRef a
newFrozenPrimRef a = runST $ newPrimRef a >>= unsafeFreezePrimRef

-- | Read the stored primitive value from the frozen reference.
indexFrozenPrimRef :: Prim a => FrozenPrimRef a -> a
indexFrozenPrimRef (FrozenPrimRef ba) = indexByteArray ba 0
{-# INLINE indexFrozenPrimRef #-}

-- | Convert an immutable primitive reference to a mutable one without copying. The original reference should not be used after the conversion.
unsafeThawPrimRef :: PrimMonad m => FrozenPrimRef a -> m (PrimRef (PrimState m) a)
unsafeThawPrimRef (FrozenPrimRef m) = PrimRef <$> unsafeThawByteArray m
{-# INLINE unsafeThawPrimRef #-}

-- | Yield a pointer to the data of a 'FrozenPrimRef'. This operation is only safe on pinned byte arrays allocated by
-- 'newPinnedPrimRef' or 'newAlignedPinnedPrimRef' and then subsequently frozen.
frozenPrimRefContents :: FrozenPrimRef a -> Ptr a
frozenPrimRefContents (FrozenPrimRef m) = castPtr $ byteArrayContents m
{-# INLINE frozenPrimRefContents #-}

--------------------------------------------------------------------------------
-- * Atomic Operations
--------------------------------------------------------------------------------

-- | Given a primitive reference, the expected old value, and the new value, perform an atomic compare and swap i.e. write the new value if the current value matches the provided old value. Returns the value of the element before the operation. Implies a full memory barrier.
casInt :: PrimMonad m => PrimRef (PrimState m) Int -> Int -> Int -> m Int
casInt (PrimRef (MutableByteArray m)) (I# old) (I# new) = primitive $ \s -> case casIntArray# m 0# old new s of
  (# s', result #) -> (# s', I# result #)

-- | Given a reference, and a value to add, atomically add the value to the element. Returns the value of the element before the operation. Implies a full memory barrier.
fetchAddInt :: PrimMonad m => PrimRef (PrimState m) Int -> Int -> m Int
fetchAddInt (PrimRef (MutableByteArray m)) (I# x) = primitive $ \s -> case fetchAddIntArray# m 0# x s of
  (# s', result #) -> (# s', I# result #)

-- | Given a reference, and a value to subtract, atomically subtract the value from the element. Returns the value of the element before the operation. Implies a full memory barrier.
fetchSubInt :: PrimMonad m => PrimRef (PrimState m) Int -> Int -> m Int
fetchSubInt (PrimRef (MutableByteArray m)) (I# x) = primitive $ \s -> case fetchSubIntArray# m 0# x s of
  (# s', result #) -> (# s', I# result #)

-- | Given a reference, and a value to bitwise and, atomically and the value with the element. Returns the value of the element before the operation. Implies a full memory barrier.
fetchAndInt :: PrimMonad m => PrimRef (PrimState m) Int -> Int -> m Int
fetchAndInt (PrimRef (MutableByteArray m)) (I# x) = primitive $ \s -> case fetchAndIntArray# m 0# x s of
  (# s', result #) -> (# s', I# result #)

-- | Given a reference, and a value to bitwise nand, atomically nand the value with the element. Returns the value of the element before the operation. Implies a full memory barrier.
fetchNandInt :: PrimMonad m => PrimRef (PrimState m) Int -> Int -> m Int
fetchNandInt (PrimRef (MutableByteArray m)) (I# x) = primitive $ \s -> case fetchNandIntArray# m 0# x s of
  (# s', result #) -> (# s', I# result #)

-- | Given a reference, and a value to bitwise or, atomically or the value with the element. Returns the value of the element before the operation. Implies a full memory barrier.
fetchOrInt :: PrimMonad m => PrimRef (PrimState m) Int -> Int -> m Int
fetchOrInt (PrimRef (MutableByteArray m)) (I# x) = primitive $ \s -> case fetchOrIntArray# m 0# x s of
  (# s', result #) -> (# s', I# result #)

-- | Given a reference, and a value to bitwise xor, atomically xor the value with the element. Returns the value of the element before the operation. Implies a full memory barrier.
fetchXorInt :: PrimMonad m => PrimRef (PrimState m) Int -> Int -> m Int
fetchXorInt (PrimRef (MutableByteArray m)) (I# x) = primitive $ \s -> case fetchXorIntArray# m 0# x s of
  (# s', result #) -> (# s', I# result #)

-- | Given a reference, read an element. Implies a full memory barrier.
atomicReadInt :: PrimMonad m => PrimRef (PrimState m) Int -> m Int
atomicReadInt (PrimRef (MutableByteArray m)) = primitive $ \s -> case atomicReadIntArray# m 0# s of
  (# s', result #) -> (# s', I# result #)

-- | Given a reference, write an element. Implies a full memory barrier.
atomicWriteInt :: PrimMonad m => PrimRef (PrimState m) Int -> Int -> m ()
atomicWriteInt (PrimRef (MutableByteArray m)) (I# x) = primitive_ $ \s -> atomicWriteIntArray# m 0# x s

instance (Prim a, Data a) => Data (FrozenPrimRef a) where
  gfoldl f z m   = z newFrozenPrimRef `f` indexFrozenPrimRef m
  toConstr _     = newFrozenPrimRefConstr
  gunfold k z c  = case constrIndex c of
    1 -> k (z newFrozenPrimRef)
    _ -> error "gunfold"
  dataTypeOf _   = frozenPrimRefDataType

newFrozenPrimRefConstr :: Constr
newFrozenPrimRefConstr = mkConstr frozenPrimRefDataType "newFrozenPrimRef" [] Prefix

frozenPrimRefDataType :: DataType
frozenPrimRefDataType = mkDataType "Data.Transient.Primitive.FrozenPrimRef" [newFrozenPrimRefConstr]
