{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Data.Primitive.MVar
-- License     : BSD2
-- Portability : non-portable
--
-- Primitive operations on @MVar@. This module provides a similar interface
-- to "Control.Concurrent.MVar". However, the functions are generalized to
-- work in any 'PrimMonad' instead of only working in 'IO'. Note that all
-- of the functions here are completely deterministic. Users of 'MVar' are
-- responsible for designing abstractions that guarantee determinism in
-- the presence of multi-threading.
module Data.Primitive.MVar
  ( MVar(..)
  , newMVar
  , isEmptyMVar
  , newEmptyMVar
  , putMVar
#if __GLASGOW_HASKELL__ >= 708
  , readMVar
#endif
  , takeMVar
  , tryPutMVar
#if __GLASGOW_HASKELL__ >= 708
  , tryReadMVar
#endif
  , tryTakeMVar
  ) where

import Control.Monad.Primitive
import Data.Primitive.Internal.Compat (isTrue#)
import GHC.Exts (MVar#,newMVar#,takeMVar#,sameMVar#,putMVar#,tryTakeMVar#,
  isEmptyMVar#,tryPutMVar#,(/=#))

#if __GLASGOW_HASKELL__ >= 708
import GHC.Exts (readMVar#,tryReadMVar#)
#endif

data MVar s a = MVar (MVar# s a)

instance Eq (MVar s a) where
  (MVar mvar1#) == (MVar mvar2#) = isTrue# (sameMVar# mvar1# mvar2#)

-- | Create a new 'MVar' that is initially empty.
newEmptyMVar :: PrimMonad m => m (MVar (PrimState m) a)
newEmptyMVar = primitive $ \ s# ->
  case newMVar# s# of
    (# s2#, svar# #) -> (# s2#, MVar svar# #)


-- | Create a new 'MVar' that holds the supplied argument.
newMVar :: PrimMonad m => a -> m (MVar (PrimState m) a)
newMVar value =
  newEmptyMVar >>= \ mvar ->
  putMVar mvar value >>
  return mvar

-- | Return the contents of the 'MVar'.  If the 'MVar' is currently
-- empty, 'takeMVar' will wait until it is full.  After a 'takeMVar',
-- the 'MVar' is left empty.
takeMVar :: PrimMonad m => MVar (PrimState m) a -> m a
takeMVar (MVar mvar#) = primitive $ \ s# -> takeMVar# mvar# s#

#if __GLASGOW_HASKELL__ >= 708
-- | Atomically read the contents of an 'MVar'.  If the 'MVar' is
-- currently empty, 'readMVar' will wait until it is full.
-- 'readMVar' is guaranteed to receive the next 'putMVar'.
--
-- This function is only available when compiling with GHC 7.8
-- or newer.
readMVar :: PrimMonad m => MVar (PrimState m) a -> m a
readMVar (MVar mvar#) = primitive $ \ s# -> readMVar# mvar# s#
#endif

-- |Put a value into an 'MVar'.  If the 'MVar' is currently full,
-- 'putMVar' will wait until it becomes empty.
putMVar :: PrimMonad m => MVar (PrimState m) a -> a -> m ()
putMVar (MVar mvar#) x = primitive_ (putMVar# mvar# x)

-- |A non-blocking version of 'takeMVar'.  The 'tryTakeMVar' function
-- returns immediately, with 'Nothing' if the 'MVar' was empty, or
-- @'Just' a@ if the 'MVar' was full with contents @a@.  After 'tryTakeMVar',
-- the 'MVar' is left empty.
tryTakeMVar :: PrimMonad m => MVar (PrimState m) a -> m (Maybe a)
tryTakeMVar (MVar m) = primitive $ \ s ->
  case tryTakeMVar# m s of
    (# s', 0#, _ #) -> (# s', Nothing #) -- MVar is empty
    (# s', _,  a #) -> (# s', Just a  #) -- MVar is full


-- |A non-blocking version of 'putMVar'.  The 'tryPutMVar' function
-- attempts to put the value @a@ into the 'MVar', returning 'True' if
-- it was successful, or 'False' otherwise.
tryPutMVar :: PrimMonad m => MVar (PrimState m) a -> a -> m Bool
tryPutMVar (MVar mvar#) x = primitive $ \ s# ->
    case tryPutMVar# mvar# x s# of
        (# s, 0# #) -> (# s, False #)
        (# s, _  #) -> (# s, True #)

#if __GLASGOW_HASKELL__ >= 708
-- | A non-blocking version of 'readMVar'.  The 'tryReadMVar' function
-- returns immediately, with 'Nothing' if the 'MVar' was empty, or
-- @'Just' a@ if the 'MVar' was full with contents @a@.
--
-- This function is only available when compiling with GHC 7.8
-- or newer.
tryReadMVar :: PrimMonad m => MVar (PrimState m) a -> m (Maybe a)
tryReadMVar (MVar m) = primitive $ \ s ->
    case tryReadMVar# m s of
        (# s', 0#, _ #) -> (# s', Nothing #)      -- MVar is empty
        (# s', _,  a #) -> (# s', Just a  #)      -- MVar is full
#endif

-- | Check whether a given 'MVar' is empty.
--
-- Notice that the boolean value returned  is just a snapshot of
-- the state of the MVar. By the time you get to react on its result,
-- the MVar may have been filled (or emptied) - so be extremely
-- careful when using this operation.   Use 'tryTakeMVar' instead if possible.
isEmptyMVar :: PrimMonad m => MVar (PrimState m) a -> m Bool
isEmptyMVar (MVar mv#) = primitive $ \ s# ->
  case isEmptyMVar# mv# s# of
    (# s2#, flg #) -> (# s2#, isTrue# (flg /=# 0#) #)
