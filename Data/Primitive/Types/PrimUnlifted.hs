{-# language CPP #-}
{-# language MagicHash #-}
{-# language TypeFamilies #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language ScopedTypeVariables #-}

-- |
-- Module      : Data.Primitive.UnliftedArray
-- Copyright   : (c) Dan Doel 2016, (c) David Feuer 2018
-- License     : BSD-style
--
-- Maintainer  : Libraries <libraries@haskell.org>
-- Portability : non-portable
--
-- Note: In a previous release, there was a `PrimUnlifted` instance for
-- `StablePtr`. However, since `StablePtr#` is not of kind `TYPE UnliftedRep`,
-- and is treated very differently by the garbage collector, this instance was
-- unsound, and it has been removed.
--
-- @since TODO

module Data.Primitive.Types.PrimUnlifted
 ( PrimUnlifted (..)
#if __GLASGOW_HASKELL__ < 800
 , Unlifted
 , toUnlifted#
 , fromUnlifted#
#endif
 ) where

import GHC.Exts

import Data.Primitive.PrimArray (PrimArray (..), MutablePrimArray (..))
import Data.Primitive.Array (Array (..), MutableArray (..))
import Data.Primitive.SmallArray (SmallArray (..), SmallMutableArray (..))
import Data.Primitive.ByteArray (ByteArray (..), MutableByteArray (..))
import Data.Primitive.MutVar (MutVar (..))
import GHC.MVar (MVar(..))
import GHC.Conc (TVar(..))
import GHC.Weak (Weak(..))
import GHC.Conc.Sync (ThreadId(..))

-- | Classifies the types that are just liftings of unlifted pointer
-- types.
--
-- For GHC versions 8.0 and later, instances should define 'Unlift',
-- 'toUnlifted#', and 'fromUnlifted#'. When compiled with earlier versions,
-- @Unlifted a@ is always a synonym for 'ArrayArray#', and instances should
-- define 'toArrayArray#' and 'fromArrayArray#', coercing the internal
-- unlifted types to and from 'ArrayArray#'.
--
-- Note: 'toArrayArray#' and 'fromArrayArray#' will eventually be
-- deprecated and removed. They should be used /only/ to define instances
-- for GHC versions before 8.0.
class PrimUnlifted a where
#if __GLASGOW_HASKELL__ >= 800
  -- Technically speaking, this is too strong; users *can* write
  -- working instances with just toArrayArray# and fromArrayArray#.
  -- But we really want to push them in the right direction for the
  -- future.
  {-# MINIMAL toUnlifted#, fromUnlifted# #-}
  -- | The unlifted pointer representation of a type. For example, since
  -- 'MutVar' is defined
  --
  -- @
  -- data MutVar s a = MutVar (MutVar# s a)
  -- @
  --
  -- we define
  --
  -- @
  -- type Unlifted (MutVar s a) = MutVar# s a
  -- @
  --
  -- @since TODO
#  if __GLASGOW_HASKELL__ >= 802
  type Unlifted a :: TYPE 'UnliftedRep
#  else
  type Unlifted a :: TYPE 'PtrRepUnlifted
#  endif
  type Unlifted a = ArrayArray#

  -- | Convert a value to its unlifted representation.
  --
  -- @since TODO
  toUnlifted# :: a -> Unlifted a
  default toUnlifted#
    :: Unlifted a ~ ArrayArray#
    => a -> Unlifted a
  toUnlifted# = toArrayArray#

  -- Convert a value from its unlifted representation.
  --
  -- @since TODO
  fromUnlifted# :: Unlifted a -> a
  default fromUnlifted#
    :: Unlifted a ~ ArrayArray#
    => Unlifted a -> a
  fromUnlifted# = fromArrayArray#
#elif __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL toArrayArray#, fromArrayArray# #-}
#endif

  -- | Convert the value to an 'ArrayArray#'.
  toArrayArray# :: a -> ArrayArray#
#if __GLASGOW_HASKELL__ >= 800
  toArrayArray# a = unsafeCoerce# (toUnlifted# a)
#endif

  -- | Convert an 'ArrayArray#' to the value.
  fromArrayArray# :: ArrayArray# -> a
#if __GLASGOW_HASKELL__ >= 800
  fromArrayArray# a# = fromUnlifted# (unsafeCoerce# a#)
#endif

#if __GLASGOW_HASKELL__ < 800
-- | For GHC versions below 8.0, we define @Unlifted a = ArrayArray#@
-- regardless of the true type structure. This is necessary because
-- earlier versions don't allow a type family to produce an unlifted
-- result. Users should always prefer 'Unlift' to 'ArrayArray#' for
-- forwards compatibility.
type Unlifted a = ArrayArray#

-- Why do we define these as functions instead of class methods with
-- defaults when __GLASGOW_HASKELL__ < 800? Some users feel virtuous
-- when they define all the methods for a class, even when there are
-- defaults. Such a user might be sorely tempted to define 'toUnlifted#'
-- and 'fromUnlifted#' the same as 'toArrayArray#' and 'fromArrayArray#'.
-- That ugly code will continue to compile and run correctly under
-- later GHC versions, and never get fixed. Since there's no point
-- in allowing that, we just don't.

-- | Convert a value to its unlifted representation.
toUnlifted# :: PrimUnlifted a => a -> Unlifted a
toUnlifted# = toArrayArray#

-- | Convert a value from its unlifted representation.
fromUnlifted# :: PrimUnlifted a => Unlifted a -> a
fromUnlifted# = fromArrayArray#
#endif

instance PrimUnlifted (Array a) where
#if __GLASGOW_HASKELL__ >= 800
  type Unlifted (Array a) = Array# a

  toUnlifted# (Array aa#) = aa#
  fromUnlifted# aa# = Array aa#
#else
  toArrayArray# (Array a#) = unsafeCoerce# a#
  fromArrayArray# aa# = Array (unsafeCoerce# aa#)
#endif

instance PrimUnlifted (MutableArray s a) where
#if __GLASGOW_HASKELL__ >= 800
  type Unlifted (MutableArray s a) = MutableArray# s a

  toUnlifted# (MutableArray ma#) = ma#
  fromUnlifted# aa# = MutableArray aa#
#else
  toArrayArray# (MutableArray ma#) = unsafeCoerce# ma#
  fromArrayArray# aa# = MutableArray (unsafeCoerce# aa#)
#endif

instance PrimUnlifted ByteArray where
#if __GLASGOW_HASKELL__ >= 800
  type Unlifted ByteArray = ByteArray#

  toUnlifted# (ByteArray ba#) = ba#
  fromUnlifted# ba# = ByteArray ba#
#else
  toArrayArray# (ByteArray ba#) = unsafeCoerce# ba#
  fromArrayArray# aa# = ByteArray (unsafeCoerce# aa#)
#endif

instance PrimUnlifted (MutableByteArray s) where
#if __GLASGOW_HASKELL__ >= 800
  type Unlifted (MutableByteArray s) = MutableByteArray# s

  toUnlifted# (MutableByteArray ba#) = ba#
  fromUnlifted# ba# = MutableByteArray ba#
#else
  toArrayArray# (MutableByteArray mba#) = unsafeCoerce# mba#
  fromArrayArray# aa# = MutableByteArray (unsafeCoerce# aa#)
#endif

-- | @since 0.6.4.0
instance PrimUnlifted (PrimArray a) where
#if __GLASGOW_HASKELL__ >= 800
  type Unlifted (PrimArray a) = ByteArray#

  toUnlifted# (PrimArray ba#) = ba#
  fromUnlifted# ba# = PrimArray ba#
#else
  toArrayArray# (PrimArray ba#) = unsafeCoerce# ba#
  fromArrayArray# aa# = PrimArray (unsafeCoerce# aa#)
#endif

-- | @since 0.6.4.0
instance PrimUnlifted (MutablePrimArray s a) where
#if __GLASGOW_HASKELL__ >= 800
  type Unlifted (MutablePrimArray s a) = MutableByteArray# s

  toUnlifted# (MutablePrimArray ba#) = ba#
  fromUnlifted# ba# = MutablePrimArray ba#
#else
  toArrayArray# (MutablePrimArray mba#) = unsafeCoerce# mba#
  fromArrayArray# aa# = MutablePrimArray (unsafeCoerce# aa#)
#endif

instance PrimUnlifted (SmallArray a) where
#if __GLASGOW_HASKELL__ >= 800
  type Unlifted (SmallArray a) = SmallArray# a

  toUnlifted# (SmallArray sa#) = sa#
  fromUnlifted# = SmallArray
#elif __GLASGOW_HASKELL__ >= 710
  toArrayArray# (SmallArray sa#) =
    unsafeCoerce# (sa# :: SmallArray# a)
  fromArrayArray# aa# =
    SmallArray (unsafeCoerce# aa# :: SmallArray# a)
#else
  -- SmallArray is a newtype
  toArrayArray# (SmallArray ar) = toArrayArray# ar
  fromArrayArray# aa# = SmallArray (fromArrayArray# aa#)
#endif

instance PrimUnlifted (SmallMutableArray s a) where
#if __GLASGOW_HASKELL__ >= 800
  type Unlifted (SmallMutableArray s a) = SmallMutableArray# s a

  toUnlifted# (SmallMutableArray sa#) = sa#
  fromUnlifted# = SmallMutableArray
#elif __GLASGOW_HASKELL__ >= 710
  toArrayArray# (SmallMutableArray sma#) =
    unsafeCoerce# (sma# :: SmallMutableArray# s a)
  fromArrayArray# aa# =
    SmallMutableArray (unsafeCoerce# aa# :: SmallMutableArray# s a)
#else
  -- SmallMutableArray is a newtype
  toArrayArray# (SmallMutableArray ar) = toArrayArray# ar
  fromArrayArray# aa# = SmallMutableArray (fromArrayArray# aa#)
#endif

instance PrimUnlifted (MutVar s a) where
#if __GLASGOW_HASKELL__ >= 800
  type Unlifted (MutVar s a) = MutVar# s a

  toUnlifted# (MutVar ref) = ref
  fromUnlifted# = MutVar
#else
  toArrayArray# (MutVar mv#) = unsafeCoerce# mv#
  fromArrayArray# aa# = MutVar (unsafeCoerce# aa#)
#endif

-- | @since 0.6.4.0
instance PrimUnlifted (MVar a) where
#if __GLASGOW_HASKELL__ >= 800
  type Unlifted (MVar a) = MVar# RealWorld a

  toUnlifted# (MVar mv#) = mv#
  fromUnlifted# = MVar
#else
  toArrayArray# (MVar mv#) = unsafeCoerce# mv#
  fromArrayArray# mv# = MVar (unsafeCoerce# mv#)
#endif

-- | @since 0.6.4.0
instance PrimUnlifted (TVar a) where
#if __GLASGOW_HASKELL__ >= 800
  type Unlifted (TVar a) = TVar# RealWorld a

  toUnlifted# (TVar tv#) = tv#
  fromUnlifted# = TVar
#else
  toArrayArray# (TVar tv#) = unsafeCoerce# tv#
  fromArrayArray# tv# = TVar (unsafeCoerce# tv#)
#endif

-- | @since 0.6.4.0
instance PrimUnlifted (Weak a) where
#if __GLASGOW_HASKELL__ >= 800
  type Unlifted (Weak a) = Weak# a

  toUnlifted# (Weak wk#) = wk#
  fromUnlifted# = Weak
#else
  toArrayArray# (Weak tv#) = unsafeCoerce# tv#
  fromArrayArray# tv# = Weak (unsafeCoerce# tv#)
#endif

-- | @since 0.6.4.0
instance PrimUnlifted ThreadId where
#if __GLASGOW_HASKELL__ >= 800
  type Unlifted ThreadId = ThreadId#

  toUnlifted# (ThreadId tid#) = tid#
  fromUnlifted# = ThreadId
#else
  toArrayArray# (ThreadId tv#) = unsafeCoerce# tv#
  fromArrayArray# tv# = ThreadId (unsafeCoerce# tv#)
#endif
