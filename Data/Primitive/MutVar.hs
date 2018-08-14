{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash, UnboxedTuples, DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- |
-- Module      : Data.Primitive.MutVar
-- Copyright   : (c) Justin Bonnar 2011, Roman Leshchinskiy 2011-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Primitive boxed mutable variables
--

module Data.Primitive.MutVar (
  MutVar(..),

  newMutVar,
  readMutVar,
  writeMutVar,
  modifyMutVar,
  modifyMutVar',

  atomicModifyMutVar',
  atomicModifyMutVar,
  atomicModifyMutVar2,
  atomicModifyMutVar2Lazy,
  FirstFieldIs,
  atomicModifyMutVar'_,
  atomicModifyMutVar_Lazy,
  atomicSwapMutVar,
  casMutVar
) where

import Control.Monad.Primitive ( PrimMonad(..), primitive_ )
import GHC.Exts ( MutVar#, sameMutVar#, newMutVar#,
                  readMutVar#, writeMutVar#, atomicModifyMutVar#,
                  casMutVar# )
import Data.Primitive.Internal.Compat ( isTrue# )
import Data.Typeable ( Typeable )
import GHC.Generics
-- TODO: Once GHC 8.8 has a base library version, we should
-- use MIN_VERSION_base here.
#if __GLASGOW_HASKELL__ >= 807
import GHC.Exts ( atomicModifyMutVar2#, atomicModifyMutVar_# )
import GHC.TypeLits (TypeError, ErrorMessage (..))
import Data.Kind (Constraint)
#endif

-- | A 'MutVar' behaves like a single-element mutable array associated
-- with a primitive state token.
data MutVar s a = MutVar (MutVar# s a)
  deriving ( Typeable )

instance Eq (MutVar s a) where
  MutVar mva# == MutVar mvb# = isTrue# (sameMutVar# mva# mvb#)

-- | Create a new 'MutVar' with the specified initial value
newMutVar :: PrimMonad m => a -> m (MutVar (PrimState m) a)
{-# INLINE newMutVar #-}
newMutVar initialValue = primitive $ \s# ->
  case newMutVar# initialValue s# of
    (# s'#, mv# #) -> (# s'#, MutVar mv# #)

-- | Read the value of a 'MutVar'
readMutVar :: PrimMonad m => MutVar (PrimState m) a -> m a
{-# INLINE readMutVar #-}
readMutVar (MutVar mv#) = primitive (readMutVar# mv#)

-- | Write a new value into a 'MutVar'
writeMutVar :: PrimMonad m => MutVar (PrimState m) a -> a -> m ()
{-# INLINE writeMutVar #-}
writeMutVar (MutVar mv#) newValue = primitive_ (writeMutVar# mv# newValue)

-- | Mutate the contents of a 'MutVar'
modifyMutVar :: PrimMonad m => MutVar (PrimState m) a -> (a -> a) -> m ()
{-# INLINE modifyMutVar #-}
modifyMutVar (MutVar mv#) g = primitive_ $ \s# ->
  case readMutVar# mv# s# of
    (# s'#, a #) -> writeMutVar# mv# (g a) s'#

-- | Strict version of 'modifyMutVar'
modifyMutVar' :: PrimMonad m => MutVar (PrimState m) a -> (a -> a) -> m ()
{-# INLINE modifyMutVar' #-}
modifyMutVar' (MutVar mv#) g = primitive_ $ \s# ->
  case readMutVar# mv# s# of
    (# s'#, a #) -> let a' = g a in a' `seq` writeMutVar# mv# a' s'#

-- | Atomically mutate the contents of a 'MutVar'. This
-- function is gratuitously lazy. Most applications should use
-- `atomicModifyMutVar'`, `atomicModifyMutVar2`, or
-- `atomicModifyMutVar'_`.
atomicModifyMutVar :: PrimMonad m => MutVar (PrimState m) a -> (a -> (a,b)) -> m b
-- TODO: Generalize the type the way we do for atomicModifyMutVar2.
-- The generalization is more annoying to implement and considerably
-- less powerful, but it can be useful to use two-field records that are not
-- actual pairs.
{-# INLINE atomicModifyMutVar #-}
atomicModifyMutVar (MutVar mv#) f = primitive $ atomicModifyMutVar# mv# f

-- | Strict version of 'atomicModifyMutVar'. This forces both the value stored
-- in the 'MutVar' as well as the value returned.
atomicModifyMutVar' :: PrimMonad m => MutVar (PrimState m) a -> (a -> (a, b)) -> m b
{-# INLINE atomicModifyMutVar' #-}
atomicModifyMutVar' mv f = do
  b <- atomicModifyMutVar mv force
  b `seq` return b
  where
    force x = case f x of
                v@(x',_) -> x' `seq` v

-- | A version of 'atomicModifyMutVar2' that does not force
-- the result of applying the function. This is /very rarely/
-- desirable. 'atomicModifyMutVar2' should /almost always/ be
-- preferred.
atomicModifyMutVar2Lazy
  :: ( PrimMonad m
     , FirstFieldIs a r )
  => MutVar (PrimState m) a -> (a -> r) -> m (a, r)
-- TODO: Use MIN_VERSION_base once we have a version
#if __GLASGOW_HASKELL__ >= 807
atomicModifyMutVar2Lazy (MutVar ref) f =
  primitive $ \s ->
    case atomicModifyMutVar2# ref f s of
      (# s', old, res #) -> (# s', (old, res) #)
#else
atomicModifyMutVar2Lazy ref f = do
  res@(_,_) <- atomicModifyMutVar ref $ \old ->
    let f_old = f old
    in (getFirst f_old, (old, f_old))
  return res
#endif
{-# INLINE atomicModifyMutVar2Lazy #-}

-- | Atomically modify the contents of a 'MutVar' using the given
-- function, and force the result of applying the function.
-- Return the old contents and the function result.
--
-- The passed function must produce a tuple or record
-- datatype that is an instance of 'Generic' and whose first
-- field has the same type as the type of the 'MutVar' contents
-- and which is not unpacked. The 'MutVar' contents are replaced
-- by the first field in the result of applying the function to the
-- old 'MutVar' contents.
--
-- Warning: this function is only safe when the 'Generic' representation
-- ('Rep') of the record (including metadata about whether it is a newtype
-- and whether fields are unpacked) accurately reflects its representation
-- in memory. Using a type with a hand-written 'Generic' instance that does not
-- do so will likely lead to segmentation faults or silent memory corruption.
--
-- Extra warning: the compatibility shim used for GHC 8.6 and below
-- is more forgiving than the (more efficient) implementation used
-- for later versions. Code using types that violate the no-newtype and
-- no-unpacked-first-field restrictions, and code using unacceptable
-- hand-written 'Generic' instances may compile and run correctly in
-- older GHC versions, but fail to compile or segfault in later versions.
--
-- === Valid signatures
--
-- @
-- atomicModifyMutVar2
--   :: MutVar RealWorld a -> (a -> (a, b)) -> IO (a, (a, b))
--
-- atomicModifyMutVar2
--   :: MutVar RealWorld a -> (a -> (a, b, c)) -> IO (a, (a, b, c))
--
-- data Foo = Foo Int Char deriving Generic
--
-- atomicModifyMutVar2
--   :: MutVar RealWorld Int -> (Int -> Foo) -> IO (Int, Foo)
-- @
--
-- === Invalid signatures
--
-- @
-- data Bar = Bar !Int Char deriving Generic
--
-- -- Invalid: when compiling with optimizations, the 'Int' will
-- -- be unboxed.
-- atomicModifyMutVar2
--   :: MutVar RealWorld Int -> (Int -> Bar) -> IO (Int, Bar)
--
-- newtype Baz = Baz Int
--
-- -- Invalid: Baz is a newtype.
-- atomicModifyMutVar2
--   :: MutVar RealWorld Int -> (Int -> Baz) -> IO (Int, Baz)
-- @
atomicModifyMutVar2
  :: ( PrimMonad m
     , FirstFieldIs a r )
  => MutVar (PrimState m) a -> (a -> r) -> m (a, r)
atomicModifyMutVar2 ref f = do
  res@(_old, !_r) <- atomicModifyMutVar2Lazy ref f
  return res

-- | Atomically modify the contents of a 'MutVar'. Returns the
-- old and new values, in that order. This function does not
-- force the new value, so it leaves a thunk in the 'MutVar'.
-- This is /very rarely/ desirable. Most applications should
-- use `atomicModifyMutVar'_` instead.
atomicModifyMutVar_Lazy
  :: PrimMonad m
  => MutVar (PrimState m) a -> (a -> a) -> m (a, a)
-- TODO: Use MIN_VERSION_base once we have a version number
#if __GLASGOW_HASKELL__ >= 807
atomicModifyMutVar_Lazy (MutVar ref) f =
  primitive $ \s ->
    case atomicModifyMutVar_# ref f s of
      (# s', old, new #) -> (# s', (old, new) #)
#else
atomicModifyMutVar_Lazy ref f = do
  res@(_, _) <- atomicModifyMutVar ref $ \old ->
    let f_old = f old
    in (f_old, (old, f_old))
  return res
#endif
{-# INLINE atomicModifyMutVar_Lazy #-}

data RSP a b = RSP a !b

-- | Atomically modify the contents of a 'MutVar'. Forces
-- the new value and returns the old and new values in that
-- order.
atomicModifyMutVar'_
  :: PrimMonad m
  => MutVar (PrimState m) a -> (a -> a) -> m (a, a)
-- TODO: Use MIN_VERSION_base once we have a version number.
#if __GLASGOW_HASKELL__ >= 807
atomicModifyMutVar'_ ref f = do
  res@(_old, !_new) <- atomicModifyMutVar_Lazy ref f
  return res
#else
atomicModifyMutVar'_ ref f = do
  -- RSP informs GHC that the new value has been forced.
  -- Most of the time, we'll avoid actually building the result
  -- pair.
  RSP old new <- atomicModifyMutVar ref $ \old ->
    let !f_old = f old
    in (f_old, RSP old f_old)
  return (old, new)
#endif
{-# INLINE atomicModifyMutVar'_ #-}

-- | @casMutVar ref old new@ atomically checks that @ref@ contains
-- @old@ and, if it does, replaces the contents with @new@. The return
-- value indicates whether the operation succeeded (@True@ means it
-- did) and also gives the latest value of the 'MutVar'. If the
-- operation succeeded, this will be the /new/ value rather than the old one,
-- unlike traditional CAS.
--
-- Caution: the test for whether the value in the 'MutVar' contains @old@
-- is done by pointer equality. That means considerable care is required
-- to make sure it really tests the right thing. Explicit @lazy@ annotations
-- will very often be required.
casMutVar
  :: PrimMonad m
  => MutVar (PrimState m) a
  -> a -> a -> m (Bool, a)
casMutVar (MutVar ref) old new = primitive $ \s ->
  case casMutVar# ref old new s of
    -- CAS succeeded (yes, 0# means success)
    (# s', 0#, latest #) -> (# s', (True, latest) #)
    -- CAS failed
    (# s', _, latest #) -> (# s', (False, latest) #)

-- | Replace the contents of a 'MutVar' with the given
-- value and return the old contents. This operation
-- neither creates nor forces any thunks.
atomicSwapMutVar
  :: PrimMonad m
  => MutVar (PrimState m) a -> a -> m a
-- TODO: There is an accepted proposal to add a primop for
-- this. Once that is implemented, we should use it here.
-- Until then, this is a faithful (but slightly less efficient)
-- imitation.
atomicSwapMutVar ref new = do
  old <- readMutVar ref
  (success, _) <- casMutVar ref old new
  if success
    then return old
    else atomicSwapMutVar ref new

-- -------------------------
--
-- Type-level machinery for atomicModifyMutVar2Lazy and
-- atomicModifyMutVar2.

-- FirstFieldIs a x checks whether x is a record type (not a newtype)
-- whose first field is not unpacked and has type a.
#if __GLASGOW_HASKELL__ < 807
-- Compat shim with lousy error messages and performance.

-- | @FirstFieldIs a x@ means that @x@ is a record datatype
-- (not a newtype) whose first field has type @a@ and is not
-- unpacked. The newtype and unpackedness restrictions are
-- only enforced under GHC 8.8 and above.
class FirstFieldIs a x where
  getFirst :: x -> a

instance (Generic x, FirstFieldIs_ a (Rep x)) => FirstFieldIs a x where
  getFirst = getFirst_ . from

class FirstFieldIs_ a r where
  getFirst_ :: r p -> a

instance FirstFieldIs_ a f => FirstFieldIs_ a (M1 i c f) where
  getFirst_ (M1 x) = getFirst_ x

instance FirstFieldIs_ a l => FirstFieldIs_ a (l :*: r) where
  getFirst_ (l :*: _) = getFirst_ l

instance a ~ c => FirstFieldIs_ a (K1 i c) where
  getFirst_ (K1 x) = x

#else
-- We have all the goodies.

-- | @FirstFieldIs a x@ means that @x@ is a record datatype
-- (not a newtype) whose first field has type @a@ and is not
-- unpacked. The newtype and unpackedness restrictions are
-- only enforced under GHC 8.8 and above.
class FirstFieldIs a x
-- We could give this a Generic superclass constraint. Should we?
-- Are there situations where adding it would make us pass an
-- unnecessary dictionary around? We can certainly add it
-- later if we want without any risk of breaking things, so
-- leaving it out is the conservative approach.

-- Why do we impose a Generic constraint when we don't really
-- need anything but the Rep? If we don't do that, and someone
-- tries to use a type that's not an instance of Generic, the
-- type error will be quite mysterious. In practice, I doubt
-- we'll actually have to pass the dictionary.
instance (Generic x, FirstFieldIs_ a (Rep x) x) => FirstFieldIs a x

type family FirstFieldIs_ a r x :: Constraint where
  -- We could dig through newtypes, but that seems too
  -- confusing.
  FirstFieldIs_ _a (M1 _ ('MetaData _ _ _ 'True) _) x =
    AMMV2Error (ShowIndType x $+$ 'Text "is a newtype.")

  -- We could dig into the unpacked representation, but that
  -- seems much too confusing.
  FirstFieldIs_ _a (M1 _ ('MetaSel _ _ _ 'DecidedUnpack) _) x =
    AMMV2Error ('Text "The first field of" $+$ ShowIndType x
                 $+$ 'Text "is unpacked.")

  FirstFieldIs_ a (M1 _ _ f) x = FirstFieldIs_ a f x

  FirstFieldIs_ _a U1 x =
    AMMV2Error (ShowIndType x $+$ 'Text "has no fields.")

  FirstFieldIs_ _a V1 x =
    AMMV2Error (ShowIndType x $+$ 'Text "is uninhabited.")

  FirstFieldIs_ _a (_ :+: _) x =
    AMMV2Error (ShowIndType x $+$ 'Text "is a sum type.")

  FirstFieldIs_ a (l :*: _) x = FirstFieldIs_ a l x

  -- When a and c are known and different, this seems to win us
  -- the better error message. I wouldn't want to bet on that being
  -- robust, but the worst that can happen is a lousy error.
  FirstFieldIs_ a (K1 _ c) x = (CheckSame a c x, a ~ c)

type ShowIndType t = 'Text "    " ':<>: 'ShowType t

infixl 5 $+$
type x $+$ y = x ':$$: 'Text "" ':$$: y

type family CheckSame a c x :: Constraint where
  CheckSame a a _x = ()
  CheckSame a c x =
    AMMV2Error ('Text "The MutVar contents have type"
                 $+$ ShowIndType a
                 $+$ 'Text "but the first field of"
                 $+$ ShowIndType x
                 $+$ 'Text "has type"
                 $+$ ShowIndType c)

type family AMMV2Error body :: Constraint where
  AMMV2Error body = TypeError
    ( 'Text "atomicModifyMutVar2 expects a function producing a record"
    ':$$: 'Text "whose first field has the same type as the MutVar contents"
    ':$$: 'Text "and which is not unpacked."
    ':$$: 'Text "" ':$$: body )
#endif
