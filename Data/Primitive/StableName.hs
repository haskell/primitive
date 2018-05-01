{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Data.Primitive.ByteArray
-- Copyright   : (c) Roman Leshchinskiy 2009-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Primitive operations on @StableName@. This differs from the module
-- "System.Mem.StableName" in that it works in both 'ST' and 'IO', as
-- well as any monad transformer stack built on top of either of
-- these. It is recommended that users read the documentation of
-- that module to learn more about the properties of a @StableName@.
module Data.Primitive.StableName
  ( StableName(..)
  , makeStableName
  , eqStableName
  ) where

import Control.Monad.Primitive
import GHC.Exts (State#,StableName#,unsafeCoerce#,makeStableName#,eqStableName#)

-- | An abstract name for an object. This supports tests for equality
-- but does not support hashing.
data StableName s a = StableName (StableName# a)

instance Eq (StableName s a) where
  (==) = eqStableName

-- | Makes a 'StableName' for an arbitrary object.  The object passed as
-- the first argument is not evaluated by 'makeStableName'.
makeStableName :: PrimMonad m => a -> m (StableName (PrimState m) a)
makeStableName a = primitive $ \ s ->
  case makeStableName# a (coerceState s) of (# s', sn #) -> (# coerceState s', StableName sn #)

-- | Equality on 'StableName' that does not require that the types of
-- the arguments match.
eqStableName :: StableName s a -> StableName s b -> Bool
eqStableName (StableName sn1) (StableName sn2) =
  case eqStableName# sn1 sn2 of
    0# -> False
    _  -> True

coerceState :: State# a -> State# b
coerceState = unsafeCoerce#

