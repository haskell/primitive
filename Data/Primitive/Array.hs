{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Data.Primitive.Array (
  Array(..), MutableArray(..),

  newArray, readArray, writeArray, indexArray,
  unsafeFreezeArray, unsafeThawArray, sameMutableArray
) where

import Control.Monad.Primitive

import GHC.Base  ( Int(..) )
import GHC.Prim

data Array a = Array (Array# a)
data MutableArray m a = MutableArray (MutableArray# (PrimState m) a)

newArray :: PrimMonad m => Int -> a -> m (MutableArray m a)
newArray (I# n#) x = primitive
   (\s# -> case newArray# n# x s# of
             (# s'#, arr# #) -> (# s'#, MutableArray arr# #))

readArray :: PrimMonad m => MutableArray m a -> Int -> m a
readArray (MutableArray arr#) (I# i#) = primitive (readArray# arr# i#)

writeArray :: PrimMonad m => MutableArray m a -> Int -> a -> m ()
writeArray (MutableArray arr#) (I# i#) x = primitive_ (writeArray# arr# i# x)

indexArray :: Array a -> Int -> (a -> b) -> b
indexArray (Array arr#) (I# i#) f = case indexArray# arr# i# of (# x #) -> f x

unsafeFreezeArray :: PrimMonad m => MutableArray m a -> m (Array a)
unsafeFreezeArray (MutableArray arr#)
  = primitive (\s# -> case unsafeFreezeArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, Array arr'# #))

unsafeThawArray :: PrimMonad m => Array a -> m (MutableArray m a)
unsafeThawArray (Array arr#)
  = primitive (\s# -> case unsafeThawArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, MutableArray arr'# #))

sameMutableArray :: MutableArray m a -> MutableArray m a -> Bool
sameMutableArray (MutableArray arr#) (MutableArray brr#)
  = sameMutableArray# arr# brr#

