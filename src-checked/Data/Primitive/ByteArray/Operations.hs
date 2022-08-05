{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Data.Primitive.ByteArray.Operations (
  readByteArray, writeByteArray, indexByteArray,
  readCharArray, writeCharArray, indexCharArray,
  copyByteArray, copyMutableByteArray,
  newByteArray
) where

import Data.Primitive.ByteArray.Internal hiding
  ( readByteArray, writeByteArray, indexByteArray
  , readCharArray, writeCharArray, indexCharArray
  , copyByteArray, copyMutableByteArray, newByteArray
  )
import Control.Monad.Primitive
import Control.Monad.ST
import Control.DeepSeq
import Data.Primitive.Types

import qualified GHC.ST as GHCST

import Foreign.C.Types
import Data.Word ( Word8 )
import Data.Bits ( (.&.), unsafeShiftR )
import GHC.Show ( intToDigit )
import qualified GHC.Exts as Exts
import GHC.Exts hiding (setByteArray#)

import Data.Typeable ( Typeable )
import Data.Data ( Data(..), mkNoRepType )
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Lib as TH

import qualified Data.Semigroup as SG
import qualified Data.Foldable as F
import qualified Data.Primitive.ByteArray.Internal as Internal

indexByteArray :: Prim a => ByteArray -> Int -> a
{-# NOINLINE indexByteArray #-}
indexByteArray = worker where
  worker :: forall b. Prim b => ByteArray -> Int -> b
  worker arr@(ByteArray arr#) i@(I# i#)
    | i < 0 = error ("indexCharArray: negative index " ++ show i)
    | i >= trueSz = error ("indexCharArray: index " ++ show i ++ " >= length " ++ show trueSz ++ ", byte length " ++ show sz)
    | otherwise = indexByteArray# arr# i#
    where
    sz = sizeofByteArray arr
    elemSz = sizeOf (undefined :: b)
    trueSz = quot sz elemSz

readByteArray
  :: (Prim a, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> m a
{-# NOINLINE readByteArray #-}
readByteArray = worker
  where
  worker :: forall n b. (Prim b, PrimMonad n) => MutableByteArray (PrimState n) -> Int -> n b
  worker (MutableByteArray arr) (I# i) = primitive
    (\st -> case i <# 0# of
      1# -> error ("readByteArray: negative index " ++ show (I# i))
      _ -> case Exts.getSizeofMutableByteArray# arr st of
        (# st', sz #) -> let trueSz = quotInt# sz elemSz in case i >=# trueSz of
          1# -> error ("readByteArray: index " ++ show (I# i) ++ " >= length " ++ show (I# trueSz) ++ ", byte length " ++ show (I# sz))
          _ -> readByteArray# arr i st'
    )
    where elemSz = sizeOf# (undefined :: b)

writeByteArray
  :: (Prim a, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> a -> m ()
{-# noinline writeByteArray #-}
writeByteArray (MutableByteArray arr) (I# i) x = primitive_
  (\st -> case i <# 0# of
    1# -> error ("writeByteArray: negative index " ++ show (I# i))
    _ -> case Exts.getSizeofMutableByteArray# arr st of
      (# st', sz #) -> let trueSz = quotInt# sz elemSz in case i >=# trueSz of
        1# -> error ("writeByteArray: index " ++ show (I# i) ++ " >= length " ++ show (I# trueSz) ++ ", byte length " ++ show (I# sz))
        _ -> writeByteArray# arr i x st'
  )
  where
  elemSz = sizeOf# x

readCharArray :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> m Char
{-# noinline readCharArray #-}
readCharArray (MutableByteArray arr) (I# i) = primitive
  (\st -> case i <# 0# of
    1# -> error ("readByteArray: negative index " ++ show (I# i))
    _ -> case Exts.getSizeofMutableByteArray# arr st of
      (# st', sz #) -> case i >=# sz of
        1# -> error ("readByteArray: index " ++ show (I# i) ++ " >= length " ++ show (I# sz))
        _ -> case Exts.readCharArray# arr i st' of
          (# st'', c #) -> (# st'', C# c #)
  )

writeCharArray
  :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> Char -> m ()
{-# noinline writeCharArray #-}
writeCharArray (MutableByteArray arr) (I# i) (C# v) = primitive_
  (\st -> case i <# 0# of
    1# -> error ("writeCharArray: negative index " ++ show (I# i))
    _ -> case Exts.getSizeofMutableByteArray# arr st of
      (# st', sz #) -> case i >=# sz of
        1# -> error ("writeCharArray: index " ++ show (I# i) ++ " >= length " ++ show (I# sz))
        _ -> Exts.writeCharArray# arr i v st'
  )

indexCharArray :: ByteArray -> Int -> Char
{-# noinline indexCharArray #-}
indexCharArray arr@(ByteArray arr#) i@(I# i#)
  | i < 0 = error ("indexCharArray: negative index " ++ show i)
  | i >= sz = error ("indexCharArray: index " ++ show i ++ " >= length " ++ show sz)
  | otherwise = C# (indexCharArray# arr# i#)
  where
  sz = sizeofByteArray arr

copyByteArray :: PrimMonad m
  => MutableByteArray (PrimState m) -- ^ destination array
  -> Int -- ^ offset into destination array
  -> ByteArray -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of elements to copy
  -> m ()
{-# noinline copyByteArray #-}
copyByteArray marr s1 arr s2 l = do
  siz <- Internal.getSizeofMutableByteArray marr
  if (s1 >= 0 && s2 >= 0 && l >= 0 && s1 + l <= siz && s2 + l <= Internal.sizeofByteArray arr)
    then Internal.copyByteArray marr s1 arr s2 l
    else error "copyByteArray: index range of out bounds"

copyMutableByteArray :: PrimMonad m
  => MutableByteArray (PrimState m) -- ^ destination array
  -> Int -- ^ offset into destination array
  -> MutableByteArray (PrimState m) -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of elements to copy
  -> m ()
{-# noinline copyMutableByteArray #-}
copyMutableByteArray marr1 s1 marr2 s2 l = do
  siz1 <- Internal.getSizeofMutableByteArray marr1
  siz2 <- Internal.getSizeofMutableByteArray marr2
  if (s1 >= 0 && s2 >= 0 && l >= 0 && s1 + l <= siz1 && s2 + l <= siz2)
    then Internal.copyMutableByteArray marr1 s1 marr2 s2 l
    else error "copyMutableByteArray: index range of out bounds"

newByteArray :: PrimMonad m => Int -> m (MutableByteArray (PrimState m))
newByteArray sz
  | sz < 0 = error "newByteArray: negative size"
  | sz > 549755813888 = error "newByteArray: size greater than 512GiB"
  | otherwise = Internal.newByteArray sz
