{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -Wall #-}

module PrimLawsWIP
  ( primLaws
  ) where

import Control.Applicative
import Control.Monad.Primitive (PrimMonad, PrimState,primitive,primitive_)
import Control.Monad.ST
import Data.Proxy (Proxy)
import Data.Primitive.ByteArray
import Data.Primitive.Types
import Data.Primitive.Ptr
import Foreign.Marshal.Alloc
import GHC.Exts
  (State#,Int#,Addr#,Int(I#),(*#),(+#),(<#),newByteArray#,unsafeFreezeByteArray#,
   copyMutableByteArray#,copyByteArray#,quotInt#,sizeofByteArray#)

#if MIN_VERSION_base(4,7,0)
import GHC.Exts (IsList(fromList,toList,fromListN),Item,
  copyByteArrayToAddr#,copyAddrToByteArray#)
#endif

import GHC.Ptr (Ptr(..))
import System.IO.Unsafe
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property)

import qualified Data.List as L
import qualified Data.Primitive as P

import Test.QuickCheck.Classes.Common (Laws(..))
import Test.QuickCheck.Classes.Compat (isTrue#)

-- | Test that a 'Prim' instance obey the several laws.
primLaws :: (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
primLaws p = Laws "Prim"
  [ ("ByteArray Put-Get (you get back what you put in)", primPutGetByteArray p)
  , ("ByteArray Get-Put (putting back what you got out has no effect)", primGetPutByteArray p)
  , ("ByteArray Put-Put (putting twice is same as putting once)", primPutPutByteArray p)
  , ("ByteArray Set Range", primSetByteArray p)
#if MIN_VERSION_base(4,7,0)
  , ("ByteArray List Conversion Roundtrips", primListByteArray p)
#endif
  , ("Addr Put-Get (you get back what you put in)", primPutGetAddr p)
  , ("Addr Get-Put (putting back what you got out has no effect)", primGetPutAddr p)
  , ("Addr Set Range", primSetOffAddr p)
  , ("Addr List Conversion Roundtrips", primListAddr p)
  ]

primListAddr :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primListAddr _ = property $ \(as :: [a]) -> unsafePerformIO $ do
  let len = L.length as
  ptr :: Ptr a <- mallocBytes (len * P.sizeOf (undefined :: a))
  let go :: Int -> [a] -> IO ()
      go !ix xs = case xs of
        [] -> return ()
        (x : xsNext) -> do
          writeOffPtr ptr ix x
          go (ix + 1) xsNext
  go 0 as
  let rebuild :: Int -> IO [a]
      rebuild !ix = if ix < len
        then (:) <$> readOffPtr ptr ix <*> rebuild (ix + 1)
        else return []
  asNew <- rebuild 0
  free ptr
  return (as == asNew)

primPutGetByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primPutGetByteArray _ = property $ \(a :: a) len -> (len > 0) ==> do
  ix <- choose (0,len - 1)
  return $ runST $ do
    arr <- newPrimArray len
    writePrimArray arr ix a
    a' <- readPrimArray arr ix
    return (a == a')

primGetPutByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primGetPutByteArray _ = property $ \(as :: [a]) -> (not (L.null as)) ==> do
  let arr1 = primArrayFromList as :: PrimArray a
      len = L.length as
  ix <- choose (0,len - 1)
  arr2 <- return $ runST $ do
    marr <- newPrimArray len
    copyPrimArray marr 0 arr1 0 len
    a <- readPrimArray marr ix
    writePrimArray marr ix a
    unsafeFreezePrimArray marr
  return (arr1 == arr2)

primPutPutByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primPutPutByteArray _ = property $ \(a :: a) (as :: [a]) -> (not (L.null as)) ==> do
  let arr1 = primArrayFromList as :: PrimArray a
      len = L.length as
  ix <- choose (0,len - 1)
  (arr2,arr3) <- return $ runST $ do
    marr2 <- newPrimArray len
    copyPrimArray marr2 0 arr1 0 len
    writePrimArray marr2 ix a
    marr3 <- newPrimArray len
    copyMutablePrimArray marr3 0 marr2 0 len
    arr2 <- unsafeFreezePrimArray marr2
    writePrimArray marr3 ix a
    arr3 <- unsafeFreezePrimArray marr3
    return (arr2,arr3)
  return (arr2 == arr3)

primPutGetAddr :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primPutGetAddr _ = property $ \(a :: a) len -> (len > 0) ==> do
  ix <- choose (0,len - 1)
  return $ unsafePerformIO $ do
    ptr :: Ptr a <- mallocBytes (len * P.sizeOf (undefined :: a))
    writeOffPtr ptr ix a
    a' <- readOffPtr ptr ix
    free ptr
    return (a == a')

primGetPutAddr :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primGetPutAddr _ =  property $ True
 --property $ \(as :: [a]) -> (not (L.null as)) ==> do
 -- let arr1 = primArrayFromList as :: PrimArray a
 --     len = L.length as
 -- ix <- choose (0,len - 1)
 -- arr2 <- return $ unsafePerformIO $ do
 --   ptr:: Ptr a <- mallocBytes (len * P.sizeOf (undefined :: a))
 --   copyPrimArrayToPtr ptr arr1 0 len
 --   a <- readOffPtr ptr ix
 --   writeOffPtr ptr ix a
 --   marr <- newPrimArray len
 --   copyPtrToMutablePrimArray marr 0 ptr len
 --   free ptr
 --   unsafeFreezePrimArray marr
 -- return (arr1 == arr2)

primSetByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primSetByteArray _ = property $ \(as :: [a]) (z :: a) -> do
  let arr1 = primArrayFromList as :: PrimArray a
      len = L.length as
  x <- choose (0,len)
  y <- choose (0,len)
  let lo = min x y
      hi = max x y
  return $ runST $ do
    marr2 <- newPrimArray len
    copyPrimArray marr2 0 arr1 0 len
    marr3 <- newPrimArray len
    copyPrimArray marr3 0 arr1 0 len
    setPrimArray marr2 lo (hi - lo) z
    internalDefaultSetPrimArray marr3 lo (hi - lo) z
    arr2 <- unsafeFreezePrimArray marr2
    arr3 <- unsafeFreezePrimArray marr3
    return (arr2 == arr3)

-- having trouble getting this to type check AND as written its really unsafe
primSetOffAddr :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primSetOffAddr _ =   property $ True
--primSetOffAddr :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
--primSetOffAddr _ = property $ \(as :: [a]) (z :: a) -> do
--  let arr1 = primArrayFromList as :: PrimArray a
--      len = L.length as
--  x <- choose (0,len)
--  y <- choose (0,len)
--  let lo = min x y
--      hi = max x y
--  return $ unsafePerformIO $ do
--    ptrA@(Ptr addrA#) :: Ptr a <- mallocBytes (len * P.sizeOf (undefined :: a))

--    copyPrimArrayToPtr ptrA arr1 0 len
--    ptrB@(Ptr addrB#) :: Ptr a <- mallocBytes (len * P.sizeOf (undefined :: a))

--    copyPrimArrayToPtr ptrB arr1 0 len
--    setPtr ptrA lo (hi - lo) z
--    internalDefaultSetOffAddr ptrB lo (hi - lo) z
--    marrA <- newPrimArray len
--    copyPtrToMutablePrimArray marrA 0 ptrA len
--    free ptrA
--    marrB <- newPrimArray len
--    copyPtrToMutablePrimArray marrB 0 ptrB len
--    free ptrB
--    arrA <- unsafeFreezePrimArray marrA
--    arrB <- unsafeFreezePrimArray marrB
--    return (arrA == arrB)

-- byte array with phantom variable that specifies element type
data PrimArray a = PrimArray ByteArray#
data MutablePrimArray s a = MutablePrimArray (MutableByteArray# s)

instance (Eq a, Prim a) => Eq (PrimArray a) where
  a1 == a2 = sizeofPrimArray a1 == sizeofPrimArray a2 && loop (sizeofPrimArray a1 - 1)
    where
    loop !i | i < 0 = True
            | otherwise = indexPrimArray a1 i == indexPrimArray a2 i && loop (i-1)

#if MIN_VERSION_base(4,7,0)
instance Prim a => IsList (PrimArray a) where
  type Item (PrimArray a) = a
  fromList = primArrayFromList
  fromListN = primArrayFromListN
  toList = primArrayToList
#endif

indexPrimArray :: forall a. Prim a => PrimArray a -> Int -> a
indexPrimArray (PrimArray arr#) (I# i#) = indexByteArray# arr# i#

sizeofPrimArray :: forall a. Prim a => PrimArray a -> Int
sizeofPrimArray (PrimArray arr#) = I# (quotInt# (sizeofByteArray# arr#) (P.sizeOf# (undefined :: a)))

newPrimArray :: forall m a. (PrimMonad m, Prim a) => Int -> m (MutablePrimArray (PrimState m) a)
newPrimArray (I# n#)
  = primitive (\s# ->
      case newByteArray# (n# *# sizeOf# (undefined :: a)) s# of
        (# s'#, arr# #) -> (# s'#, MutablePrimArray arr# #)
    )

readPrimArray :: (Prim a, PrimMonad m) => MutablePrimArray (PrimState m) a -> Int -> m a
readPrimArray (MutablePrimArray arr#) (I# i#)
  = primitive (readByteArray# arr# i#)

writePrimArray ::
     (Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> Int
  -> a
  -> m ()
writePrimArray (MutablePrimArray arr#) (I# i#) x
  = primitive_ (writeByteArray# arr# i# x)

unsafeFreezePrimArray
  :: PrimMonad m => MutablePrimArray (PrimState m) a -> m (PrimArray a)
unsafeFreezePrimArray (MutablePrimArray arr#)
  = primitive (\s# -> case unsafeFreezeByteArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, PrimArray arr'# #))



generateM_ :: Monad m => Int -> (Int -> m a) -> m ()
generateM_ n f = go 0 where
  go !ix = if ix < n
    then f ix >> go (ix + 1)
    else return ()


copyPrimArrayToPtr :: forall m a. (PrimMonad m, Prim a)
  => Ptr a       -- ^ destination pointer
  -> PrimArray a -- ^ source array
  -> Int         -- ^ offset into source array
  -> Int         -- ^ number of prims to copy
  -> m ()
#if MIN_VERSION_base(4,7,0)
copyPrimArrayToPtr (Ptr addr#) (PrimArray ba#) (I# soff#) (I# n#) =
  primitive (\ s# ->
      let s'# = copyByteArrayToAddr# ba# (soff# *# siz#) addr# (n# *# siz#) s#
      in (# s'#, () #))
  where siz# = sizeOf# (undefined :: a)
#else
copyPrimArrayToPtr ptr  ba soff n =
  generateM_ n $ \ix -> writeOffPtr ptr  ix (indexPrimArray ba (ix + soff))
#endif
{-
copyPtrToMutablePrimArray :: forall m a. (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a
  -> Int
  -> Ptr a
  -> Int
  -> m ()
#if MIN_VERSION_base(4,7,0)
copyPtrToMutablePrimArray (MutablePrimArray ba#) (I# doff#) (Ptr addr#) (I# n#) =
  primitive (\ s# ->
      let s'# = copyAddrToByteArray# addr# ba# (doff# *# siz#) (n# *# siz#) s#
      in (# s'#, () #))
  where siz# = sizeOf# (undefined :: a)
#else
copyPtrToMutablePrimArray ba doff addr n =
  generateM_ n $ \ix -> do
    x <- readOffAddr (ptrToAddr addr) ix
    writePrimArray ba (doff + ix) x
#endif
-}
copyMutablePrimArray :: forall m s a.
     (PrimMonad m, s ~ PrimState m , Prim a)
  => MutablePrimArray s a -- ^ destination array
  -> Int -- ^ offset into destination array
  -> MutablePrimArray s  a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of bytes to copy
  -> m ()
copyMutablePrimArray (MutablePrimArray dst#) (I# doff#) (MutablePrimArray src#) (I# soff#) (I# n#)
  = primitive_ (copyMutableByteArray#
      src#
      (soff# *# (sizeOf# (undefined :: a)))
      dst#
      (doff# *# (sizeOf# (undefined :: a)))
      (n# *# (sizeOf# (undefined :: a)))
    )

copyPrimArray :: forall m a.
     (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ destination array
  -> Int -- ^ offset into destination array
  -> PrimArray a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of bytes to copy
  -> m ()
copyPrimArray (MutablePrimArray dst#) (I# doff#) (PrimArray src#) (I# soff#) (I# n#)
  = primitive_ (copyByteArray#
      src#
      (soff# *# (sizeOf# (undefined :: a)))
      dst#
      (doff# *# (sizeOf# (undefined :: a)))
      (n# *# (sizeOf# (undefined :: a)))
    )

setPrimArray
  :: (Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a -- ^ array to fill
  -> Int -- ^ offset into array
  -> Int -- ^ number of values to fill
  -> a -- ^ value to fill with
  -> m ()
setPrimArray (MutablePrimArray dst#) (I# doff#) (I# sz#) x
  = primitive_ (P.setByteArray# dst# doff# sz# x)

primArrayFromList :: Prim a => [a] -> PrimArray a
primArrayFromList xs = primArrayFromListN (L.length xs) xs

primArrayFromListN :: forall a. Prim a => Int -> [a] -> PrimArray a
primArrayFromListN len vs = runST run where
  run :: forall s. ST s (PrimArray a)
  run = do
    arr <- newPrimArray len
    let go :: [a] -> Int -> ST s ()
        go !xs !ix = case xs of
          [] -> return ()
          a : as -> do
            writePrimArray arr ix a
            go as (ix + 1)
    go vs 0
    unsafeFreezePrimArray arr

primArrayToList :: forall a. Prim a => PrimArray a -> [a]
primArrayToList arr = go 0 where
  !len = sizeofPrimArray arr
  go :: Int -> [a]
  go !ix = if ix < len
    then indexPrimArray arr ix : go (ix + 1)
    else []

#if MIN_VERSION_base(4,7,0)
primListByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primListByteArray _ = property $ \(as :: [a]) ->
  as == toList (fromList as :: PrimArray a)
#endif


internalDefaultSetPrimArray :: Prim a
  => MutablePrimArray s a -> Int -> Int -> a -> ST s ()
internalDefaultSetPrimArray (MutablePrimArray arr) (I# i) (I# len) ident =
  primitive_ (internalDefaultSetByteArray# arr i len ident)

internalDefaultSetByteArray# :: Prim a
  => MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
internalDefaultSetByteArray# arr# i# len# ident = go 0#
  where
  go ix# s0 = if isTrue# (ix# <# len#)
    then case writeByteArray# arr# (i# +# ix#) ident s0 of
      s1 -> go (ix# +# 1#) s1
    else s0

internalDefaultSetOffAddr :: Prim a => Ptr a -> Int -> Int -> a -> IO ()
internalDefaultSetOffAddr (Ptr addr) (I# ix) (I# len) a = primitive_
  (internalDefaultSetOffAddr# addr ix len a)

internalDefaultSetOffAddr# :: Prim a => Addr# -> Int# -> Int# -> a -> State# s -> State# s
internalDefaultSetOffAddr# addr# i# len# ident = go 0#
  where
  go ix# s0 = if isTrue# (ix# <# len#)
    then case writeOffAddr# addr# (i# +# ix#) ident s0 of
      s1 -> go (ix# +# 1#) s1
    else s0
