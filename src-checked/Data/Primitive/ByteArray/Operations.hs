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
) where

import Data.Primitive.ByteArray.Internal hiding
  ( readByteArray, writeByteArray, indexByteArray
  , readCharArray, writeCharArray, indexCharArray
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

-- | Read a primitive value from the byte array. The offset is given in
-- elements of type @a@ rather than in bytes.
--
-- /Note:/ this function does not do bounds checking.
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

-- | Read a primitive value from the byte array. The offset is given in
-- elements of type @a@ rather than in bytes.
--
-- /Note:/ this function does not do bounds checking.
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

-- | Write a primitive value to the byte array. The offset is given in
-- elements of type @a@ rather than in bytes.
--
-- /Note:/ this function does not do bounds checking.
writeByteArray
  :: (Prim a, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> a -> m ()
{-# NOLINE writeByteArray #-}
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

-- | Read an 8-bit element from the byte array, interpreting it as a
-- Latin-1-encoded character. The offset is given in bytes.
--
-- /Note:/ this function does not do bounds checking.
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

-- | Write a character to the byte array, encoding it with Latin-1 as
-- a single byte. Behavior is undefined for codepoints outside of the
-- ASCII and Latin-1 blocks. The offset is given in bytes.
--
-- /Note:/ this function does not do bounds checking.
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

-- | Read an 8-bit element from the byte array, interpreting it as a
-- Latin-1-encoded character. The offset is given in bytes.
--
-- /Note:/ this function does not do bounds checking.
indexCharArray :: ByteArray -> Int -> Char
{-# noinline indexCharArray #-}
indexCharArray arr@(ByteArray arr#) i@(I# i#)
  | i < 0 = error ("indexCharArray: negative index " ++ show i)
  | i >= sz = error ("indexCharArray: index " ++ show i ++ " >= length " ++ show sz)
  | otherwise = C# (indexCharArray# arr# i#)
  where
  sz = sizeofByteArray arr
