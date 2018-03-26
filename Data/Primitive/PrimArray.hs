{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -Wall #-}

module Data.Primitive.PrimArray
  ( -- * Types
    PrimArray(..)
  , MutablePrimArray(..)
    -- * Allocation
  , newPrimArray
    -- * Element Access
  , readPrimArray
  , writePrimArray
  , indexPrimArray
    -- * Freezing and Thawing
  , unsafeFreezePrimArray
  , unsafeThawPrimArray
    -- * Block Operations
  , copyPrimArray
  , copyMutablePrimArray
  , copyPrimArrayToPtr
  , copyMutablePrimArrayToPtr
  , setPrimArray
    -- * Folding
  , foldrPrimArray
  , foldrPrimArray'
  , foldlPrimArray
  , foldlPrimArray'
  , foldlPrimArrayM'
    -- * Traversing
  , mapPrimArray
  , mapPrimArrayM
  , mapPrimArrayP
  , imapPrimArray
  , imapPrimArrayM
  , imapPrimArrayP
    -- * Information
  , sameMutablePrimArray
  , getSizeofMutablePrimArray
  , sizeofPrimArray
  ) where

import GHC.Prim
import GHC.Exts (isTrue#,IsList(..))
import GHC.Base ( Int(..) )
import GHC.Ptr
import Data.Primitive
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.List as L
import qualified Data.Primitive.ByteArray as PB
import qualified Data.Primitive.Types as PT

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup(..))
import qualified Data.Semigroup as SG
#endif

-- | Primitive arrays
data PrimArray a = PrimArray ByteArray#

-- | Mutable primitive arrays associated with a primitive state token
data MutablePrimArray s a = MutablePrimArray (MutableByteArray# s)

sameByteArray :: ByteArray# -> ByteArray# -> Bool
sameByteArray ba1 ba2 =
    case reallyUnsafePtrEquality# (unsafeCoerce# ba1 :: ()) (unsafeCoerce# ba2 :: ()) of
#if __GLASGOW_HASKELL__ >= 708
      r -> isTrue# r
#else
      1# -> True
      0# -> False
#endif

instance (Eq a, Prim a) => Eq (PrimArray a) where
  a1@(PrimArray ba1#) == a2@(PrimArray ba2#)
    | sameByteArray ba1# ba2# = True
    | I# (sizeofByteArray# ba1#) /= I# (sizeofByteArray# ba2#) = False
    | otherwise = loop (sizeofPrimArray a1 - 1)
    where 
    loop !i
      | i < 0 = True
      | otherwise = indexPrimArray a1 i == indexPrimArray a2 i && loop (i-1)

instance Prim a => IsList (PrimArray a) where
  type Item (PrimArray a) = a
  fromList xs = primArrayFromListN (L.length xs) xs
  fromListN = primArrayFromListN
  toList = primArrayToList

instance (Show a, Prim a) => Show (PrimArray a) where
  showsPrec p a = showParen (p > 10) $
    showString "fromListN " . shows (sizeofPrimArray a) . showString " "
      . shows (primArrayToList a)

die :: String -> String -> a
die fun problem = error $ "Data.Primitive.PrimArray." ++ fun ++ ": " ++ problem

primArrayFromListN :: forall a. Prim a => Int -> [a] -> PrimArray a
primArrayFromListN len vs = runST run where
  run :: forall s. ST s (PrimArray a)
  run = do
    arr <- newPrimArray len
    let go :: [a] -> Int -> ST s ()
        go [] !ix = if ix == len
          then return ()
          else die "fromListN" "list length less than specified size"
        go (a : as) !ix = if ix < len
          then do
            writePrimArray arr ix a
            go as (ix + 1)
          else die "fromListN" "list length greater than specified size"
    go vs 0
    unsafeFreezePrimArray arr

primArrayToList :: forall a. Prim a => PrimArray a -> [a]
primArrayToList = foldrPrimArray (:) []

primArrayToByteArray :: PrimArray a -> PB.ByteArray
primArrayToByteArray (PrimArray x) = PB.ByteArray x

-- only used internally, no error checking
{-# INLINE primArrayFromBackwardsListN #-}
primArrayFromBackwardsListN :: forall a. Prim a => Int -> [a] -> PrimArray a
primArrayFromBackwardsListN len vs = runST run where
  run :: forall s. ST s (PrimArray a)
  run = do
    arr <- newPrimArray len
    let go :: [a] -> Int -> ST s ()
        go [] !_ = return ()
        go (a : as) !ix = do
          writePrimArray arr ix a
          go as (ix - 1)
    go vs (len - 1)
    unsafeFreezePrimArray arr

byteArrayToPrimArray :: ByteArray -> PrimArray a
byteArrayToPrimArray (PB.ByteArray x) = PrimArray x

#if MIN_VERSION_base(4,9,0)
instance Semigroup (PrimArray a) where
  x <> y = byteArrayToPrimArray (primArrayToByteArray x SG.<> primArrayToByteArray y)
  sconcat = byteArrayToPrimArray . SG.sconcat . fmap primArrayToByteArray
  stimes i arr = byteArrayToPrimArray (stimes i (primArrayToByteArray arr))
#endif

instance Monoid (PrimArray a) where
  mempty = emptyPrimArray
#if !(MIN_VERSION_base(4,11,0))
  mappend x y = byteArrayToPrimArray (mappend (primArrayToByteArray x) (primArrayToByteArray y))
#endif
  mconcat = byteArrayToPrimArray . mconcat . map primArrayToByteArray

emptyPrimArray :: PrimArray a
{-# NOINLINE emptyPrimArray #-}
emptyPrimArray = runST $ primitive $ \s0# -> case newByteArray# 0# s0# of
  (# s1#, arr# #) -> case unsafeFreezeByteArray# arr# s1# of
    (# s2#, arr'# #) -> (# s2#, PrimArray arr'# #)

newPrimArray :: forall m a. (PrimMonad m, Prim a) => Int -> m (MutablePrimArray (PrimState m) a)
{-# INLINE newPrimArray #-}
newPrimArray (I# n#)
  = primitive (\s# -> 
      case newByteArray# (n# *# sizeOf# (undefined :: a)) s# of
        (# s'#, arr# #) -> (# s'#, MutablePrimArray arr# #)
    )

readPrimArray :: (Prim a, PrimMonad m) => MutablePrimArray (PrimState m) a -> Int -> m a
{-# INLINE readPrimArray #-}
readPrimArray (MutablePrimArray arr#) (I# i#)
  = primitive (readByteArray# arr# i#)

writePrimArray ::
     (Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> Int
  -> a
  -> m ()
{-# INLINE writePrimArray #-}
writePrimArray (MutablePrimArray arr#) (I# i#) x
  = primitive_ (writeByteArray# arr# i# x)

-- | Copy part of a mutable array into another mutable array.
--   In the case that the destination and
--   source arrays are the same, the regions may overlap.
copyMutablePrimArray :: forall m a.
     (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ destination array
  -> Int -- ^ offset into destination array
  -> MutablePrimArray (PrimState m) a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of bytes to copy
  -> m ()
{-# INLINE copyMutablePrimArray #-}
copyMutablePrimArray (MutablePrimArray dst#) (I# doff#) (MutablePrimArray src#) (I# soff#) (I# n#)
  = primitive_ (copyMutableByteArray#
      src# 
      (soff# *# (sizeOf# (undefined :: a)))
      dst#
      (doff# *# (sizeOf# (undefined :: a)))
      (n# *# (sizeOf# (undefined :: a)))
    )

-- | Copy part of an array into another mutable array.
copyPrimArray :: forall m a.
     (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ destination array
  -> Int -- ^ offset into destination array
  -> PrimArray a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of bytes to copy
  -> m ()
{-# INLINE copyPrimArray #-}
copyPrimArray (MutablePrimArray dst#) (I# doff#) (PrimArray src#) (I# soff#) (I# n#)
  = primitive_ (copyByteArray#
      src# 
      (soff# *# (sizeOf# (undefined :: a)))
      dst#
      (doff# *# (sizeOf# (undefined :: a)))
      (n# *# (sizeOf# (undefined :: a)))
    )

-- | Copy a slice of an immutable primitive array to an address.
--   The offset and length are given in elements of type @a@.
--   This function assumes that the 'Prim' instance of @a@
--   agrees with the 'Storable' instance.
copyPrimArrayToPtr :: forall m a. (PrimMonad m, Prim a)
  => Ptr a -- ^ destination pointer
  -> PrimArray a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of prims to copy
  -> m ()
{-# INLINE copyPrimArrayToPtr #-}
copyPrimArrayToPtr (Ptr addr#) (PrimArray ba#) (I# soff#) (I# n#) =
    primitive (\ s# ->
        let s'# = copyByteArrayToAddr# ba# (soff# *# siz#) addr# (n# *# siz#) s#
        in (# s'#, () #))
  where siz# = sizeOf# (undefined :: a)

-- | Copy a slice of an immutable primitive array to an address.
--   The offset and length are given in elements of type @a@.
--   This function assumes that the 'Prim' instance of @a@
--   agrees with the 'Storable' instance.
copyMutablePrimArrayToPtr :: forall m a. (PrimMonad m, Prim a)
  => Ptr a -- ^ destination pointer
  -> MutablePrimArray (PrimState m) a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of prims to copy
  -> m ()
{-# INLINE copyMutablePrimArrayToPtr #-}
copyMutablePrimArrayToPtr (Ptr addr#) (MutablePrimArray mba#) (I# soff#) (I# n#) =
    primitive (\ s# ->
        let s'# = copyMutableByteArrayToAddr# mba# (soff# *# siz#) addr# (n# *# siz#) s#
        in (# s'#, () #))
  where siz# = sizeOf# (undefined :: a)

-- | Fill a slice of a mutable byte array with a value.
setPrimArray
  :: (Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a -- ^ array to fill
  -> Int -- ^ offset into array
  -> Int -- ^ number of values to fill
  -> a -- ^ value to fill with
  -> m ()
{-# INLINE setPrimArray #-}
setPrimArray (MutablePrimArray dst#) (I# doff#) (I# sz#) x
  = primitive_ (PT.setByteArray# dst# doff# sz# x)

-- | Get the size of the mutable array.
getSizeofMutablePrimArray :: forall m a. (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ array
  -> m Int
{-# INLINE getSizeofMutablePrimArray #-}
getSizeofMutablePrimArray (MutablePrimArray arr#)
  = primitive (\s# -> 
      case getSizeofMutableByteArray# arr# s# of
        (# s'#, sz# #) -> (# s'#, I# (quotInt# sz# (sizeOf# (undefined :: a))) #)
    )

-- | Check if the two arrays refer to the same memory block.
sameMutablePrimArray :: MutablePrimArray s a -> MutablePrimArray s a -> Bool
{-# INLINE sameMutablePrimArray #-}
sameMutablePrimArray (MutablePrimArray arr#) (MutablePrimArray brr#)
  = isTrue# (sameMutableByteArray# arr# brr#)

-- | Convert a mutable byte array to an immutable one without copying. The
-- array should not be modified after the conversion.
unsafeFreezePrimArray
  :: PrimMonad m => MutablePrimArray (PrimState m) a -> m (PrimArray a)
{-# INLINE unsafeFreezePrimArray #-}
unsafeFreezePrimArray (MutablePrimArray arr#)
  = primitive (\s# -> case unsafeFreezeByteArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, PrimArray arr'# #))

-- | Convert an immutable array to a mutable one without copying. The
-- original array should not be used after the conversion.
unsafeThawPrimArray
  :: PrimMonad m => PrimArray a -> m (MutablePrimArray (PrimState m) a)
{-# INLINE unsafeThawPrimArray #-}
unsafeThawPrimArray (PrimArray arr#)
  = primitive (\s# -> (# s#, MutablePrimArray (unsafeCoerce# arr#) #))

-- | Read a primitive value from the array.
indexPrimArray :: forall a. Prim a => PrimArray a -> Int -> a
{-# INLINE indexPrimArray #-}
indexPrimArray (PrimArray arr#) (I# i#) = indexByteArray# arr# i#

sizeofPrimArray :: forall a. Prim a => PrimArray a -> Int
{-# INLINE sizeofPrimArray #-}
sizeofPrimArray (PrimArray arr#) = I# (quotInt# (sizeofByteArray# arr#) (sizeOf# (undefined :: a)))

-- | Lazy right-associated fold over the elements of a 'PrimArray'.
{-# INLINE foldrPrimArray #-}
foldrPrimArray :: forall a b. Prim a => (a -> b -> b) -> b -> PrimArray a -> b
foldrPrimArray f z arr = go 0
  where
    !sz = sizeofPrimArray arr
    go !i
      | sz > i = f (indexPrimArray arr i) (go (i+1))
      | otherwise = z

-- | Strict right-associated fold over the elements of a 'PrimArray'.
{-# INLINE foldrPrimArray' #-}
foldrPrimArray' :: forall a b. Prim a => (a -> b -> b) -> b -> PrimArray a -> b
foldrPrimArray' f z0 arr = go (sizeofPrimArray arr - 1) z0
  where
    go !i !acc
      | i < 0 = acc
      | otherwise = go (i - 1) (f (indexPrimArray arr i) acc)

-- | Lazy left-associated fold over the elements of a 'PrimArray'.
{-# INLINE foldlPrimArray #-}
foldlPrimArray :: forall a b. Prim a => (b -> a -> b) -> b -> PrimArray a -> b
foldlPrimArray f z arr = go (sizeofPrimArray arr - 1)
  where
    go !i
      | i < 0 = z
      | otherwise = f (go (i - 1)) (indexPrimArray arr i)

-- | Strict left-associated fold over the elements of a 'PrimArray'.
{-# INLINE foldlPrimArray' #-}
foldlPrimArray' :: forall a b. Prim a => (b -> a -> b) -> b -> PrimArray a -> b
foldlPrimArray' f z0 arr = go 0 z0
  where
    !sz = sizeofPrimArray arr
    go !i !acc
      | i < sz = go (i + 1) (f acc (indexPrimArray arr i))
      | otherwise = acc

-- | Strict left-associated fold over the elements of a 'PrimArray'.
{-# INLINE foldlPrimArrayM' #-}
foldlPrimArrayM' :: (Prim a, Monad m) => (b -> a -> m b) -> b -> PrimArray a -> m b
foldlPrimArrayM' f z0 arr = go 0 z0
  where
    !sz = sizeofPrimArray arr
    go !i !acc1
      | i < sz = do
          acc2 <- f acc1 (indexPrimArray arr i)
          go (i + 1) acc2
      | otherwise = return acc1

-- | Monadic map over a 'PrimArray'.
{-# INLINE mapPrimArrayM #-}
mapPrimArrayM :: (Monad m, Prim a, Prim b)
  => (a -> m b)
  -> PrimArray a
  -> m (PrimArray b)
mapPrimArrayM f arr = go 0 []
  where
  !sz = sizeofPrimArray arr
  go !ix !xs
    | ix < sz = do
        !x <- f (indexPrimArray arr ix)
        go (ix + 1) (x : xs)
    | otherwise = return (primArrayFromBackwardsListN sz xs)

-- | Monadic map over a 'PrimArray' where the monad allows
--   primitive actions.
{-# INLINE mapPrimArrayP #-}
mapPrimArrayP :: (PrimBase m, Prim a, Prim b)
  => (a -> m b)
  -> PrimArray a
  -> m (PrimArray b)
mapPrimArrayP = unsafeMapPrimArrayP

-- This function is not exported. It only produces correct
-- results when the monad is sufficiently affine.
{-# INLINE unsafeMapPrimArrayP #-}
unsafeMapPrimArrayP :: (PrimMonad m, Prim a, Prim b)
  => (a -> m b)
  -> PrimArray a
  -> m (PrimArray b)
unsafeMapPrimArrayP f arr = do
  let !sz = sizeofPrimArray arr
  marr <- newPrimArray sz
  let go !ix = if ix < sz
        then do
          b <- f (indexPrimArray arr ix)
          writePrimArray marr ix b
          go (ix + 1)
        else return ()
  go 0
  unsafeFreezePrimArray marr

{-# INLINE mapPrimArray #-}
mapPrimArray :: (Prim a, Prim b)
  => (a -> b)
  -> PrimArray a
  -> PrimArray b
mapPrimArray f arr = runST $ do
  let !sz = sizeofPrimArray arr
  marr <- newPrimArray sz
  let go !ix = if ix < sz
        then do
          let b = f (indexPrimArray arr ix)
          writePrimArray marr ix b
          go (ix + 1)
        else return ()
  go 0
  unsafeFreezePrimArray marr

{-# INLINE imapPrimArray #-}
imapPrimArray :: (Prim a, Prim b)
  => (Int -> a -> b)
  -> PrimArray a
  -> PrimArray b
imapPrimArray f arr = runST $ do
  let !sz = sizeofPrimArray arr
  marr <- newPrimArray sz
  let go !ix = if ix < sz
        then do
          let b = f ix (indexPrimArray arr ix)
          writePrimArray marr ix b
          go (ix + 1)
        else return ()
  go 0
  unsafeFreezePrimArray marr

{-# INLINE imapPrimArrayM #-}
imapPrimArrayM :: (Prim a, Prim b, Monad m)
  => (Int -> a -> m b)
  -> PrimArray a
  -> m (PrimArray b)
imapPrimArrayM f arr = go 0 []
  where
  !sz = sizeofPrimArray arr
  go !ix !xs
    | ix < sz = do
        !x <- f ix (indexPrimArray arr ix)
        go (ix + 1) (x : xs)
    | otherwise = return (primArrayFromBackwardsListN sz xs)

{-# INLINE imapPrimArrayP #-}
imapPrimArrayP :: (Prim a, Prim b, PrimBase m)
  => (Int -> a -> m b)
  -> PrimArray a
  -> m (PrimArray b)
imapPrimArrayP f arr = do
  let !sz = sizeofPrimArray arr
  marr <- newPrimArray sz
  let go !ix
        | ix < sz = do
            writePrimArray marr ix =<< f ix (indexPrimArray arr ix)
            go (ix + 1)
        | otherwise = return ()
  go 0
  unsafeFreezePrimArray marr

generatePrimArray :: Prim a
  => Int -- ^ length
  -> (Int -> a) -- ^ element from index
  -> PrimArray a
generatePrimArray len f = runST $ do
  marr <- newPrimArray len
  let go !ix = if ix < len
        then do
          writePrimArray marr ix (f ix)
          go (ix + 1)
        else return ()
  go 0
  unsafeFreezePrimArray marr

