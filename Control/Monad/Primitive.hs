{-# LANGUAGE MagicHash, UnboxedTuples, TypeFamilies #-}

-- |
-- Module      : Control.Monad.Primitive
-- Copyright   : (c) Roman Leshchinskiy 2009
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
-- 
-- Primitive state-transformer monads
--

module Control.Monad.Primitive ( PrimMonad(..), RealWorld, primitive_ ) where

import GHC.Prim   ( State#, RealWorld )
import GHC.IOBase ( IO(..) )
import GHC.ST     ( ST(..) )

-- | Class of primitive state-transformer monads
class Monad m => PrimMonad m where
  -- | State token type
  type PrimState m

  -- | Execute a primitive operation
  primitive :: (State# (PrimState m) -> (# State# (PrimState m), a #)) -> m a

-- | Execute a primitive operation with no result
primitive_ :: PrimMonad m
              => (State# (PrimState m) -> State# (PrimState m)) -> m ()
{-# INLINE primitive_ #-}
primitive_ f = primitive (\s# -> (# f s#, () #))

instance PrimMonad IO where
  type PrimState IO = RealWorld
  primitive = IO

instance PrimMonad (ST s) where
  type PrimState (ST s) = s
  primitive = ST

