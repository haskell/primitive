{-# LANGUAGE CPP, MagicHash, UnboxedTuples, DeriveDataTypeable, BangPatterns, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Primitive.Array
-- Copyright   : (c) Roman Leshchinskiy 2009-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Primitive arrays of boxed values.
--

module Data.Primitive.Array (
  Array(..), MutableArray(..),

  newArray, readArray, writeArray, indexArray, indexArrayM, indexArray##,
  freezeArray, thawArray, runArray,
  unsafeFreezeArray, unsafeThawArray, sameMutableArray,
  copyArray, copyMutableArray,
  cloneArray, cloneMutableArray,
  sizeofArray, sizeofMutableArray,
  fromListN, fromList,
  arrayFromListN, arrayFromList,
  mapArray',
  traverseArrayP
) where

import Control.DeepSeq
import Control.Monad.Primitive
import Data.Data (mkNoRepType)

import GHC.Base  ( Int(..) )
import GHC.Exts
#if (MIN_VERSION_base(4,7,0))
  hiding (toList)
#endif
import qualified GHC.Exts as Exts
#if (MIN_VERSION_base(4,7,0))
import GHC.Exts (fromListN, fromList)
#endif

import Data.Typeable ( Typeable )
import Data.Data
  (Data(..), DataType, mkDataType, Constr, mkConstr, Fixity(..), constrIndex)
import Data.Primitive.Internal.Compat ( isTrue# )

import Control.Monad.ST(ST,runST)

import Control.Applicative
import Control.Monad (MonadPlus(..), when)
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import qualified Data.Foldable as Foldable
#if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip
#endif
import Data.Foldable (Foldable(..), toList)
#if !(MIN_VERSION_base(4,8,0))
import Data.Traversable (Traversable(..))
import Data.Monoid
#endif
#if MIN_VERSION_base(4,9,0)
import qualified GHC.ST as GHCST
import qualified Data.Foldable as F
import Data.Semigroup
#endif
import Data.Functor.Identity
#if MIN_VERSION_base(4,10,0)
import GHC.Exts (runRW#)
#elif MIN_VERSION_base(4,9,0)
import GHC.Base (runRW#)
#endif

import Text.Read (Read (..), parens, prec)
import Text.ParserCombinators.ReadPrec (ReadPrec)
import qualified Text.ParserCombinators.ReadPrec as RdPrc
import Text.ParserCombinators.ReadP

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import Data.Functor.Classes (Eq1(..),Ord1(..),Show1(..),Read1(..))
#endif
import Control.Monad (liftM2)

import Data.Functor.Compose
import Control.Monad (join)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT,runMaybeT))
import Control.Monad.Trans.State.Strict (StateT(StateT),State,runStateT)
import Control.Monad.Trans.Reader (ReaderT(ReaderT,runReaderT),Reader)

-- | Boxed arrays
data Array a = Array
  { array# :: Array# a }
  deriving ( Typeable )

#if MIN_VERSION_deepseq(1,4,3)
instance NFData1 Array where
  liftRnf r = Foldable.foldl' (\_ -> r) ()
#endif

instance NFData a => NFData (Array a) where
  rnf = Foldable.foldl' (\_ -> rnf) ()

-- | Mutable boxed arrays associated with a primitive state token.
data MutableArray s a = MutableArray
  { marray# :: MutableArray# s a }
  deriving ( Typeable )

sizeofArray :: Array a -> Int
sizeofArray a = I# (sizeofArray# (array# a))
{-# INLINE sizeofArray #-}

sizeofMutableArray :: MutableArray s a -> Int
sizeofMutableArray a = I# (sizeofMutableArray# (marray# a))
{-# INLINE sizeofMutableArray #-}

-- | Create a new mutable array of the specified size and initialise all
-- elements with the given value.
--
-- /Note:/ this function does not check if the input is non-negative.
newArray :: PrimMonad m => Int -> a -> m (MutableArray (PrimState m) a)
{-# INLINE newArray #-}
newArray (I# n#) x = primitive
   (\s# -> case newArray# n# x s# of
             (# s'#, arr# #) ->
               let ma = MutableArray arr#
               in (# s'# , ma #))

-- | Read a value from the array at the given index.
--
-- /Note:/ this function does not do bounds checking.
readArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> m a
{-# INLINE readArray #-}
readArray arr (I# i#) = primitive (readArray# (marray# arr) i#)

-- | Write a value to the array at the given index.
--
-- /Note:/ this function does not do bounds checking.
writeArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> a -> m ()
{-# INLINE writeArray #-}
writeArray arr (I# i#) x = primitive_ (writeArray# (marray# arr) i# x)

-- | Read a value from the immutable array at the given index.
--
-- /Note:/ this function does not do bounds checking.
indexArray :: Array a -> Int -> a
{-# INLINE indexArray #-}
indexArray arr (I# i#) = case indexArray# (array# arr) i# of (# x #) -> x

-- | Read a value from the immutable array at the given index, returning
-- the result in an unboxed unary tuple. This is currently used to implement
-- folds.
--
-- /Note:/ this function does not do bounds checking.
indexArray## :: Array a -> Int -> (# a #)
indexArray## arr (I# i) = indexArray# (array# arr) i
{-# INLINE indexArray## #-}

-- | Monadically read a value from the immutable array at the given index.
-- This allows us to be strict in the array while remaining lazy in the read
-- element which is very useful for collective operations. Suppose we want to
-- copy an array. We could do something like this:
--
-- > copy marr arr ... = do ...
-- >                        writeArray marr i (indexArray arr i) ...
-- >                        ...
--
-- But since primitive arrays are lazy, the calls to 'indexArray' will not be
-- evaluated. Rather, @marr@ will be filled with thunks each of which would
-- retain a reference to @arr@. This is definitely not what we want!
--
-- With 'indexArrayM', we can instead write
--
-- > copy marr arr ... = do ...
-- >                        x <- indexArrayM arr i
-- >                        writeArray marr i x
-- >                        ...
--
-- Now, indexing is executed immediately although the returned element is
-- still not evaluated.
--
-- /Note:/ this function does not do bounds checking.
indexArrayM :: Monad m => Array a -> Int -> m a
{-# INLINE indexArrayM #-}
indexArrayM arr (I# i#)
  = case indexArray# (array# arr) i# of (# x #) -> return x

-- | Create an immutable copy of a slice of an array.
--
-- This operation makes a copy of the specified section, so it is safe to
-- continue using the mutable array afterward.
freezeArray
  :: PrimMonad m
  => MutableArray (PrimState m) a -- ^ source
  -> Int                          -- ^ offset
  -> Int                          -- ^ length
  -> m (Array a)
{-# INLINE freezeArray #-}
freezeArray (MutableArray ma#) (I# off#) (I# len#) =
  primitive $ \s -> case freezeArray# ma# off# len# s of
    (# s', a# #) -> (# s', Array a# #)

-- | Convert a mutable array to an immutable one without copying. The
-- array should not be modified after the conversion.
unsafeFreezeArray :: PrimMonad m => MutableArray (PrimState m) a -> m (Array a)
{-# INLINE unsafeFreezeArray #-}
unsafeFreezeArray arr
  = primitive (\s# -> case unsafeFreezeArray# (marray# arr) s# of
                        (# s'#, arr'# #) ->
                          let a = Array arr'#
                          in (# s'#, a #))

-- | Create a mutable array from a slice of an immutable array.
--
-- This operation makes a copy of the specified slice, so it is safe to use the
-- immutable array afterward.
thawArray
  :: PrimMonad m
  => Array a -- ^ source
  -> Int     -- ^ offset
  -> Int     -- ^ length
  -> m (MutableArray (PrimState m) a)
{-# INLINE thawArray #-}
thawArray (Array a#) (I# off#) (I# len#) =
  primitive $ \s -> case thawArray# a# off# len# s of
    (# s', ma# #) -> (# s', MutableArray ma# #)

-- | Convert an immutable array to an mutable one without copying. The
-- immutable array should not be used after the conversion.
unsafeThawArray :: PrimMonad m => Array a -> m (MutableArray (PrimState m) a)
{-# INLINE unsafeThawArray #-}
unsafeThawArray a
  = primitive (\s# -> case unsafeThawArray# (array# a) s# of
                        (# s'#, arr'# #) ->
                          let ma = MutableArray arr'#
                          in (# s'#, ma #))

-- | Check whether the two arrays refer to the same memory block.
sameMutableArray :: MutableArray s a -> MutableArray s a -> Bool
{-# INLINE sameMutableArray #-}
sameMutableArray arr brr
  = isTrue# (sameMutableArray# (marray# arr) (marray# brr))

-- | Copy a slice of an immutable array to a mutable array.
--
-- /Note:/ this function does not do bounds or overlap checking.
copyArray :: PrimMonad m
          => MutableArray (PrimState m) a    -- ^ destination array
          -> Int                             -- ^ offset into destination array
          -> Array a                         -- ^ source array
          -> Int                             -- ^ offset into source array
          -> Int                             -- ^ number of elements to copy
          -> m ()
{-# INLINE copyArray #-}
#if __GLASGOW_HASKELL__ > 706
-- NOTE: copyArray# and copyMutableArray# are slightly broken in GHC 7.6.* and earlier
copyArray (MutableArray dst#) (I# doff#) (Array src#) (I# soff#) (I# len#)
  = primitive_ (copyArray# src# soff# dst# doff# len#)
#else
copyArray !dst !doff !src !soff !len = go 0
  where
    go i | i < len = do
                       x <- indexArrayM src (soff+i)
                       writeArray dst (doff+i) x
                       go (i+1)
         | otherwise = return ()
#endif

-- | Copy a slice of a mutable array to another array. The two arrays must
-- not be the same when using this library with GHC versions 7.6 and older.
-- In GHC 7.8 and newer, overlapping arrays will behave correctly.
--
-- /Note:/ The order of arguments is different from that of 'copyMutableArray#'. The primop
-- has the source first while this wrapper has the destination first.
--
-- /Note:/ this function does not do bounds or overlap checking.
copyMutableArray :: PrimMonad m
          => MutableArray (PrimState m) a    -- ^ destination array
          -> Int                             -- ^ offset into destination array
          -> MutableArray (PrimState m) a    -- ^ source array
          -> Int                             -- ^ offset into source array
          -> Int                             -- ^ number of elements to copy
          -> m ()
{-# INLINE copyMutableArray #-}
#if __GLASGOW_HASKELL__ > 706
-- NOTE: copyArray# and copyMutableArray# are slightly broken in GHC 7.6.* and earlier
copyMutableArray (MutableArray dst#) (I# doff#)
                 (MutableArray src#) (I# soff#) (I# len#)
  = primitive_ (copyMutableArray# src# soff# dst# doff# len#)
#else
copyMutableArray !dst !doff !src !soff !len = go 0
  where
    go i | i < len = do
                       x <- readArray src (soff+i)
                       writeArray dst (doff+i) x
                       go (i+1)
         | otherwise = return ()
#endif

-- | Return a newly allocated Array with the specified subrange of the
-- provided Array.
--
-- /Note:/ The provided Array should contain the full subrange
-- specified by the two Ints, but this is not checked.
cloneArray :: Array a -- ^ source array
           -> Int     -- ^ offset into destination array
           -> Int     -- ^ number of elements to copy
           -> Array a
{-# INLINE cloneArray #-}
cloneArray (Array arr#) (I# off#) (I# len#)
  = case cloneArray# arr# off# len# of arr'# -> Array arr'#

-- | Return a newly allocated MutableArray. with the specified subrange of
-- the provided MutableArray. The provided MutableArray should contain the
-- full subrange specified by the two Ints, but this is not checked.
--
-- /Note:/ The provided Array should contain the full subrange
-- specified by the two Ints, but this is not checked.
cloneMutableArray :: PrimMonad m
        => MutableArray (PrimState m) a -- ^ source array
        -> Int                          -- ^ offset into destination array
        -> Int                          -- ^ number of elements to copy
        -> m (MutableArray (PrimState m) a)
{-# INLINE cloneMutableArray #-}
cloneMutableArray (MutableArray arr#) (I# off#) (I# len#) = primitive
   (\s# -> case cloneMutableArray# arr# off# len# s# of
             (# s'#, arr'# #) -> (# s'#, MutableArray arr'# #))

emptyArray :: Array a
emptyArray =
  runST $ newArray 0 (die "emptyArray" "impossible") >>= unsafeFreezeArray
{-# NOINLINE emptyArray #-}

#if !MIN_VERSION_base(4,9,0)
createArray
  :: Int
  -> a
  -> (forall s. MutableArray s a -> ST s ())
  -> Array a
createArray 0 _ _ = emptyArray
createArray n x f = runArray $ do
  mary <- newArray n x
  f mary
  pure mary

runArray
  :: (forall s. ST s (MutableArray s a))
  -> Array a
runArray m = runST $ m >>= unsafeFreezeArray

#else /* Below, runRW# is available. */

-- This low-level business is designed to work with GHC's worker-wrapper
-- transformation. A lot of the time, we don't actually need an Array
-- constructor. By putting it on the outside, and being careful about
-- how we special-case the empty array, we can make GHC smarter about this.
-- The only downside is that separately created 0-length arrays won't share
-- their Array constructors, although they'll share their underlying
-- Array#s.
createArray
  :: Int
  -> a
  -> (forall s. MutableArray s a -> ST s ())
  -> Array a
createArray 0 _ _ = Array (emptyArray# (# #))
createArray n x f = runArray $ do
  mary <- newArray n x
  f mary
  pure mary

-- |
-- Execute the monadic action(s) and freeze the resulting array.
runArray
  :: (forall s. ST s (MutableArray s a))
  -> Array a
runArray m = Array (runArray# m)

runArray#
  :: (forall s. ST s (MutableArray s a))
  -> Array# a
runArray# m = case runRW# $ \s ->
  case unST m s of { (# s', MutableArray mary# #) ->
  unsafeFreezeArray# mary# s'} of (# _, ary# #) -> ary#

unST :: ST s a -> State# s -> (# State# s, a #)
unST (GHCST.ST f) = f

emptyArray# :: (# #) -> Array# a
emptyArray# _ = case emptyArray of Array ar -> ar
{-# NOINLINE emptyArray# #-}
#endif


die :: String -> String -> a
die fun problem = error $ "Data.Primitive.Array." ++ fun ++ ": " ++ problem

arrayLiftEq :: (a -> b -> Bool) -> Array a -> Array b -> Bool
arrayLiftEq p a1 a2 = sizeofArray a1 == sizeofArray a2 && loop (sizeofArray a1 - 1)
  where loop i | i < 0     = True
               | (# x1 #) <- indexArray## a1 i
               , (# x2 #) <- indexArray## a2 i
               , otherwise = p x1 x2 && loop (i-1)

instance Eq a => Eq (Array a) where
  a1 == a2 = arrayLiftEq (==) a1 a2

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
-- | @since 0.6.4.0
instance Eq1 Array where
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
  liftEq = arrayLiftEq
#else
  eq1 = arrayLiftEq (==)
#endif
#endif

instance Eq (MutableArray s a) where
  ma1 == ma2 = isTrue# (sameMutableArray# (marray# ma1) (marray# ma2))

arrayLiftCompare :: (a -> b -> Ordering) -> Array a -> Array b -> Ordering
arrayLiftCompare elemCompare a1 a2 = loop 0
  where
  mn = sizeofArray a1 `min` sizeofArray a2
  loop i
    | i < mn
    , (# x1 #) <- indexArray## a1 i
    , (# x2 #) <- indexArray## a2 i
    = elemCompare x1 x2 `mappend` loop (i+1)
    | otherwise = compare (sizeofArray a1) (sizeofArray a2)

-- | Lexicographic ordering. Subject to change between major versions.
instance Ord a => Ord (Array a) where
  compare a1 a2 = arrayLiftCompare compare a1 a2

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
-- | @since 0.6.4.0
instance Ord1 Array where
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
  liftCompare = arrayLiftCompare
#else
  compare1 = arrayLiftCompare compare
#endif
#endif

instance Foldable Array where
  -- Note: we perform the array lookups eagerly so we won't
  -- create thunks to perform lookups even if GHC can't see
  -- that the folding function is strict.
  foldr f = \z !ary ->
    let
      !sz = sizeofArray ary
      go i
        | i == sz = z
        | (# x #) <- indexArray## ary i
        = f x (go (i+1))
    in go 0
  {-# INLINE foldr #-}
  foldl f = \z !ary ->
    let
      go i
        | i < 0 = z
        | (# x #) <- indexArray## ary i
        = f (go (i-1)) x
    in go (sizeofArray ary - 1)
  {-# INLINE foldl #-}
  foldr1 f = \ !ary ->
    let
      !sz = sizeofArray ary - 1
      go i =
        case indexArray## ary i of
          (# x #) | i == sz -> x
                  | otherwise -> f x (go (i+1))
    in if sz < 0
       then die "foldr1" "empty array"
       else go 0
  {-# INLINE foldr1 #-}
  foldl1 f = \ !ary ->
    let
      !sz = sizeofArray ary - 1
      go i =
        case indexArray## ary i of
          (# x #) | i == 0 -> x
                  | otherwise -> f (go (i - 1)) x
    in if sz < 0
       then die "foldl1" "empty array"
       else go sz
  {-# INLINE foldl1 #-}
#if MIN_VERSION_base(4,6,0)
  foldr' f = \z !ary ->
    let
      go i !acc
        | i == -1 = acc
        | (# x #) <- indexArray## ary i
        = go (i-1) (f x acc)
    in go (sizeofArray ary - 1) z
  {-# INLINE foldr' #-}
  foldl' f = \z !ary ->
    let
      !sz = sizeofArray ary
      go i !acc
        | i == sz = acc
        | (# x #) <- indexArray## ary i
        = go (i+1) (f acc x)
    in go 0 z
  {-# INLINE foldl' #-}
#endif
#if MIN_VERSION_base(4,8,0)
  null a = sizeofArray a == 0
  {-# INLINE null #-}
  length = sizeofArray
  {-# INLINE length #-}
  maximum ary | sz == 0   = die "maximum" "empty array"
              | (# frst #) <- indexArray## ary 0
              = go 1 frst
   where
     sz = sizeofArray ary
     go i !e
       | i == sz = e
       | (# x #) <- indexArray## ary i
       = go (i+1) (max e x)
  {-# INLINE maximum #-}
  minimum ary | sz == 0   = die "minimum" "empty array"
              | (# frst #) <- indexArray## ary 0
              = go 1 frst
   where sz = sizeofArray ary
         go i !e
           | i == sz = e
           | (# x #) <- indexArray## ary i
           = go (i+1) (min e x)
  {-# INLINE minimum #-}
  sum = foldl' (+) 0
  {-# INLINE sum #-}
  product = foldl' (*) 1
  {-# INLINE product #-}
#endif

newtype STA a = STA {_runSTA :: forall s. MutableArray# s a -> ST s (Array a)}

runSTA :: Int -> STA a -> Array a
runSTA !sz = \ (STA m) -> runST $ newArray_ sz >>= \ ar -> m (marray# ar)
{-# INLINE runSTA #-}

newArray_ :: Int -> ST s (MutableArray s a)
newArray_ !n = newArray n badTraverseValue

badTraverseValue :: a
badTraverseValue = die "traverse" "bad indexing"
{-# NOINLINE badTraverseValue #-}

instance Traversable Array where
  traverse f = traverseArray f
  {-# INLINE traverse #-}

traverseArray
  :: Applicative f
  => (a -> f b)
  -> Array a
  -> f (Array b)
traverseArray f = \ !ary ->
  let
    !len = sizeofArray ary
    go !i
      | i == len = pure $ STA $ \mary -> unsafeFreezeArray (MutableArray mary)
      | (# x #) <- indexArray## ary i
      = liftA2 (\b (STA m) -> STA $ \mary ->
                  writeArray (MutableArray mary) i b >> m mary)
               (f x) (go (i + 1))
  in if len == 0
     then pure emptyArray
     else runSTA len <$> go 0
{-# INLINE [1] traverseArray #-}

-- Note on rewrite rules for traverse. Some types admit a traversal that
-- outperforms the general one that works with all applicatives. Such types
-- include IO and ST as well as any type constructed by layering sufficiently
-- affine monad transformers on top of IO or ST. This also includes the types
-- that correspond to such monad transformers.
--
-- For example, MaybeT is sufficiently affine. Consequently, for
-- 'MaybeT (ST s)' and 'MaybeT IO', the traversal offered by traverseArrayP is
-- semantically equivalent to traverseArray, but its tail-recursiveness
-- and lack of closure allocations mean that it performs better. This also
-- gives us a faster traversal for 'Maybe', since we can hoist an arbitrary
-- 'Maybe' into 'MaybeT (ST s)', perform the faster traversal, and then run
-- the effectful computaton to get back to a 'Maybe'.
--
-- Rewrite rule are not provided for the lazy State type or for any variant
-- of Writer. Use of these types is the types is likely to build up thunks
-- on the heap anyway.
{-# RULES
"traverse/ST" forall (f :: a -> ST s b). traverseArray f =
   traverseArrayP f
"traverse/IO" forall (f :: a -> IO b). traverseArray f =
   traverseArrayP f
"traverse/Maybe" forall (f :: a -> Maybe b). traverseArray f =
   (\xs -> runST (runMaybeT (traverseArrayP (MaybeT . return . f) xs)))
"traverse/Either" forall (f :: a -> Either e b). traverseArray f =
   traverseEither f
"traverse/State" forall (f :: a -> State s b). traverseArray f =
   (\xs -> StateT (\s0 -> Identity (runST (runStateT (traverseArrayP (hoistState . f) xs) s0))))
"traverse/Reader" forall (f :: a -> Reader r b). traverseArray f =
   (\xs -> ReaderT (\s0 -> Identity (runST (runReaderT (traverseArrayP (hoistReader . f) xs) s0))))
 #-}
#if MIN_VERSION_base(4,8,0)
{-# RULES
"traverse/Id" forall (f :: a -> Identity b). traverseArray f =
   (coerce :: (Array a -> Array (Identity b))
           -> Array a -> Identity (Array b)) (fmap f)
 #-}
#endif


{-# RULES
"traverse/MaybeT/init" forall (f :: a -> MaybeT m b). traverseArray f = runTraverseMonad (initMaybeT (traverseArray f) f)
"traverse/MaybeT/pop" forall (t :: TraverseMonad p n (MaybeT m) a b). runTraverseMonad t = runTraverseMonad (popMaybeT t)
"traverse/IO/run" forall (t :: TraverseMonad p n IO a b). runTraverseMonad t = finalizeTraverseMonadIO t
"traverse/ST/run" forall (t :: TraverseMonad p n (ST s) a b). runTraverseMonad t = finalizeTraverseMonadST t
 #-}

-- "traverse/Maybe/run" forall (t :: TraverseMonad p n Maybe a b). runTraverseMonad t = (\xs -> runST (getCompose (finalizeTraverseMonadST (finalMaybe t) xs)))

-- type variables: full, inner (starts as empty), outer (starts with everything), from, to
data TraverseMonad p n m a b = TraverseMonad
  (Array a -> p (Array b))
  -- original traversal, used if we run into an unrecognized monad or monad transformer
  (a -> p b)
  -- original traverse function
  (forall x. (forall y z. (y -> z) -> m y -> m z) -> m (n x) -> p x)
  -- given an implementation of fmap for m, convert the split stack back to the original type
  (forall x. (forall y z. (y -> z) -> m y -> m z) -> p x -> m (n x))
  -- convert original type to split stack
  (forall x y. (forall z. z -> m z) -> (forall w z. m w -> (w -> m z) -> m z) -> m (n x) -> (x -> m (n y)) -> m (n y))
  -- lift monadic bind, needs both pure and bind of underlying monad
  (forall x. (forall y z. (y -> z) -> m y -> m z) -> m x -> m (n x))
  -- lift, given fmap
  -- (forall s x. (forall f y. Applicative f => m (f y) -> f (m y)) -> m (n (ST s x)) -> ST s (m (n x)))
  (forall s x y. (forall z. z -> (m z)) -> (forall w z s'. ST s' (m w) -> (w -> ST s' (m z)) -> (ST s' (m z))) -> ST s (m (n x)) -> (x -> ST s (m (n y))) -> ST s (m (n y)))
  -- traverse in ST
  -- this is needed to make base monads other than IO or ST (like Maybe) work

runTraverseMonad :: TraverseMonad p n m a b -> Array a -> p (Array b)
runTraverseMonad (TraverseMonad f _ _ _ _ _ _) = f
{-# NOINLINE[1] runTraverseMonad #-}

initMaybeT ::
     (Array a -> MaybeT m (Array b))
  -> (a -> MaybeT m b)
  -> TraverseMonad (MaybeT m) Maybe m a b
initMaybeT f t = TraverseMonad f t (\_ -> MaybeT) (\_ -> runMaybeT)
  (\pure' bind' m g -> bind' m $ \mx -> case mx of
    Nothing -> pure' Nothing
    Just x -> g x
  )
  (\fmap' x -> fmap' Just x)
  (\pure' bind' m g -> bind' m $ \mx -> case mx of
    Nothing -> return (pure' Nothing)
    Just x -> g x
  )

popMaybeT ::
     TraverseMonad p n (MaybeT m) a b
  -> TraverseMonad p (Compose Maybe n) m a b
popMaybeT (TraverseMonad f t trans transBack liftBind lift' liftBindST) = TraverseMonad f t
  (\fmap' x -> trans (liftMapMaybeT fmap') (MaybeT (fmap' getCompose x)))
  (\fmap' x -> fmap' Compose (runMaybeT (transBack (liftMapMaybeT fmap') x)))
  (\pure' bind' m g -> fmapFromPureBind pure' bind' Compose
    (runMaybeT (liftBind (liftPureMaybeT pure') (liftBindMaybeT pure' bind') (MaybeT (fmapFromPureBind pure' bind' getCompose m)) (\x -> MaybeT (fmapFromPureBind pure' bind' getCompose (g x)))))
  )
  (\fmap' x -> fmap' Compose (runMaybeT (lift' (liftMapMaybeT fmap') (MaybeT (fmap' Just x)))))
  (\pure' bind' m g -> fmapFromPureBindST pure' bind' Compose
    (fmap runMaybeT (liftBindST (liftPureMaybeT pure') (liftBindMaybeT_ST pure' bind') (fmap MaybeT (fmapFromPureBindST pure' bind' getCompose m)) (\x -> fmap MaybeT (fmapFromPureBindST pure' bind' getCompose (g x)))))
  )

finalMaybe ::
     TraverseMonad p n Maybe a b
  -> TraverseMonad (Compose (ST s) p) (Compose Maybe n) (ST s) a b
finalMaybe (TraverseMonad f t trans transBack liftBind lift' liftBindST) = TraverseMonad
  (\arr -> Compose (return (f arr)))
  (\a -> Compose (return (t a)))
  (\_ x -> Compose (fmap (\(Compose mn) -> trans fmap mn) x))
  (\_ (Compose x) -> fmap (\p -> Compose (transBack fmap p)) x)
  (\_ _ v g -> do
    Compose mn <- v
    r <- liftBindST pure bindMaybeST (return mn) (\y -> fmap getCompose (g y))
    return (Compose r)
  )
  (\_ x -> fmap (Compose . lift' fmap . Just) x)
  (\_ _ m g -> return (fmap Compose (liftBindST Just bindMaybeST (fmap getCompose (join m)) (\y -> fmap getCompose (join (g y))))))

bindMaybeST :: ST s (Maybe a) -> (a -> ST s (Maybe b)) -> ST s (Maybe b)
bindMaybeST sm g = do
  m <- sm
  case m of
    Nothing -> pure Nothing
    Just a -> g a

-- finalMaybe ::
--      TraverseMonad p n Maybe a b
--   -> TraverseMonad (Compose (ST s) p) (Compose Maybe n) (ST s) a b
-- finalMaybe (TraverseMonad f t trans transBack liftBind lift' trav) = TraverseMonad
--   (\arr -> Compose (return (f arr)))
--   (\a -> Compose (return (t a)))
--   (\_ x -> Compose (fmap (\(Compose mn) -> trans fmap mn) x))
--   (\_ (Compose x) -> fmap (\p -> Compose (transBack fmap p)) x)
--   (\_ _ v g -> do
--     Compose mn <- v
--     let y = fmapTwiceFromPureBind (lift' fmap . Just) (liftBind pure (>>=)) g mn
--     fmap (Compose . joinTwiceFromBind (liftBind pure (>>=)) . fmapTwiceFromPureBind (lift' fmap . Just) (liftBind pure (>>=)) getCompose) (trav sequenceA y)
--   )
--   (\_ x -> fmap (Compose . lift' fmap . Just) x)
--   (\_ -> error "uheotn")

liftPureMaybeT :: (forall a. a -> m a) -> b -> MaybeT m b
liftPureMaybeT pure' = MaybeT . pure' . Just

liftPureMaybeT_ST :: (forall a. a -> m a) -> b -> ST s (MaybeT m b)
liftPureMaybeT_ST pure' = return . MaybeT . pure' . Just

liftMapMaybeT ::
     (forall a b. (a -> b) -> m a -> m b)
  -> (x -> y) -> MaybeT m x -> MaybeT m y
liftMapMaybeT fmap' f (MaybeT m) = MaybeT (fmap' (fmap f) m)

liftBindMaybeT ::
     (forall a. a -> m a)
  -> (forall a b. m a -> (a -> m b) -> m b)
  -> MaybeT m x -> (x -> MaybeT m y) -> MaybeT m y
liftBindMaybeT pure' bind' (MaybeT m) g = MaybeT $ bind' m $ \mx -> case mx of
    Nothing -> pure' Nothing
    Just x -> runMaybeT (g x)

liftBindMaybeT_ST ::
     (forall a. a -> m a)
  -> (forall a b. ST s (m a) -> (a -> ST s (m b)) -> ST s (m b))
  -> ST s (MaybeT m x) -> (x -> ST s (MaybeT m y)) -> ST s (MaybeT m y)
liftBindMaybeT_ST pure' bind' sma g = fmap MaybeT $ bind' (fmap runMaybeT sma) $ \ma -> case ma of
  Nothing -> return (pure' Nothing)
  Just a -> fmap runMaybeT (g a)

fmapFromPureBind ::
     (forall x. x -> m x)
  -> (forall x y. m x -> (x -> m y) -> m y)
  -> (a -> b) -> m a -> m b
fmapFromPureBind pure' bind' f ma = bind' ma (\z -> pure' (f z))

fmapFromPureBindST ::
     (forall x. x -> m x)
  -> (forall x y. ST s (m x) -> (x -> ST s (m y)) -> ST s (m y))
  -> (a -> b) -> ST s (m a) -> ST s (m b)
fmapFromPureBindST pure' bind' f ma = bind' ma (\z -> return (pure' (f z)))

fmapTwiceFromPureBind ::
     (forall x. x -> m (n x))
  -> (forall x y. m (n x) -> (x -> m (n y)) -> m (n y))
  -> (a -> b) -> m (n a) -> m (n b)
fmapTwiceFromPureBind pure' bind' f ma = bind' ma (\z -> pure' (f z))

joinTwiceFromBind ::
     (forall x y. m (n x) -> (x -> m (n y)) -> m (n y))
  -> m (n (m (n a)))
  -> m (n a)
joinTwiceFromBind bind' ma = bind' ma id


finalizeTraverseMonadIO :: forall p n a b. TraverseMonad p n IO a b -> Array a -> p (Array b)
finalizeTraverseMonadIO (TraverseMonad _ f trans transBack liftBind lift' _) = \ !ary ->
  trans fmap
  ( let
      !sz = sizeofArray ary
      go :: Int -> MutableArray RealWorld b -> IO (n (Array b))
      go !i !mary
        | i == sz = lift' fmap (unsafeFreezeArray mary)
        | otherwise =
            liftBind pure (>>=) (lift' fmap (indexArrayM ary i)) $ \a -> 
            liftBind pure (>>=) (transBack fmap (f a)) $ \b -> 
            liftBind pure (>>=) (lift' fmap (writeArray mary i b)) $ \_ -> 
            go (i + 1) mary
    in liftBind pure (>>=) (lift' fmap (newArray sz badTraverseValue)) $ \mary ->
       go 0 mary
  )
{-# INLINE finalizeTraverseMonadIO #-}

finalizeTraverseMonadST :: forall s p n a b. TraverseMonad p n (ST s) a b -> Array a -> p (Array b)
finalizeTraverseMonadST (TraverseMonad _ f trans transBack liftBind lift' _) = \ !ary ->
  trans fmap
  ( let
      !sz = sizeofArray ary
      go :: Int -> MutableArray s b -> ST s (n (Array b))
      go !i !mary
        | i == sz = lift' fmap (unsafeFreezeArray mary)
        | otherwise =
            liftBind pure (>>=) (lift' fmap (indexArrayM ary i)) $ \a -> 
            liftBind pure (>>=) (transBack fmap (f a)) $ \b -> 
            liftBind pure (>>=) (lift' fmap (writeArray mary i b)) $ \_ -> 
            go (i + 1) mary
    in liftBind pure (>>=) (lift' fmap (newArray sz badTraverseValue)) $ \mary ->
       go 0 mary
  )
{-# INLINE finalizeTraverseMonadST #-}



-- finalizeTraverseMonadMaybe :: forall p n a b. TraverseMonad p n Maybe a b -> Array a -> p (Array b)
-- finalizeTraverseMonadMaybe (TraverseMonad _ f trans transBack liftBind lift') = \ !ary ->
--   runST
--   ( let
--       !sz = sizeofArray ary
--       go :: Int -> MutableArray s b -> ST s (Maybe (n (Array b)))
--       go !i !mary
--         | i == sz = do
--             result <- unsafeFreezeArray mary
--             return (lift' fmap (Just result))
--         | otherwise = case indexArray## ary i of
--             (# a #) -> 
--               liftBind pure (>>=) (transBack fmap (f a)) $ \b -> 
--               liftBind pure (>>=) (lift' fmap (writeArray mary i b)) $ \_ -> 
--               go (i + 1) mary
--     in do mary <- newArray sz badTraverseValue
--           mnary <- go 0 mary
--           return (trans fmap mnary)
--   )
-- {-# INLINE finalizeTraverseMonadMaybe #-}

-- This is only used internally in a rewrite rule. Ideally, this function
-- would live in transformers.
hoistState :: Monad m => State s a -> StateT s m a
hoistState (StateT f) = StateT (return . runIdentity . f)
{-# INLINE hoistState #-}

-- This is only used internally in a rewrite rule. Ideally, this function
-- would live in transformers.
hoistReader :: Monad m => Reader r a -> ReaderT r m a
hoistReader (ReaderT f) = ReaderT (return . runIdentity . f)
{-# INLINE hoistReader #-}

-- This is required for Either's rewrite rule. It would be
-- much more concise to use ExceptT just like we use the
-- other monad transformers in the other rewrite rules, but
-- ExceptT isn't available on older versions of transformers.
traverseEither :: forall e a b.
     (a -> Either e b)
  -> Array a
  -> Either e (Array b)
traverseEither f = \ !ary ->
  let
    !sz = sizeofArray ary
    go :: forall s. Int -> MutableArray s b -> ST s (Either e (Array b))
    go !i !mary
      | i == sz = do
          r <- unsafeFreezeArray mary
          return (Right r)
      | otherwise = do
          a <- indexArrayM ary i
          case f a of
            Left e -> return (Left e)
            Right b -> do
              writeArray mary i b
              go (i + 1) mary
  in runST $ do
    mary <- newArray sz badTraverseValue
    go 0 mary
{-# INLINE traverseEither #-}


-- | This is the fastest, most straightforward way to traverse
-- an array, but it only works correctly with a sufficiently
-- "affine" 'PrimMonad' instance. In particular, it must only produce
-- *one* result array. 'Control.Monad.Trans.List.ListT'-transformed
-- monads, for example, will not work right at all.
traverseArrayP
  :: PrimMonad m
  => (a -> m b)
  -> Array a
  -> m (Array b)
traverseArrayP f = \ !ary ->
  let
    !sz = sizeofArray ary
    go !i !mary
      | i == sz
      = unsafeFreezeArray mary
      | otherwise
      = do
          a <- indexArrayM ary i
          b <- f a
          writeArray mary i b
          go (i + 1) mary
  in do
    mary <- newArray sz badTraverseValue
    go 0 mary
{-# INLINE traverseArrayP #-}

-- | Strict map over the elements of the array.
mapArray' :: (a -> b) -> Array a -> Array b
mapArray' f a =
  createArray (sizeofArray a) (die "mapArray'" "impossible") $ \mb ->
    let go i | i == sizeofArray a
             = return ()
             | otherwise
             = do x <- indexArrayM a i
                  -- We use indexArrayM here so that we will perform the
                  -- indexing eagerly even if f is lazy.
                  let !y = f x
                  writeArray mb i y >> go (i+1)
     in go 0
{-# INLINE mapArray' #-}

-- | Create an array from a list of a known length. If the length
--   of the list does not match the given length, this throws an exception.
arrayFromListN :: Int -> [a] -> Array a
arrayFromListN n l =
  createArray n (die "fromListN" "uninitialized element") $ \sma ->
    let go !ix [] = if ix == n
          then return ()
          else die "fromListN" "list length less than specified size"
        go !ix (x : xs) = if ix < n
          then do
            writeArray sma ix x
            go (ix+1) xs
          else die "fromListN" "list length greater than specified size"
    in go 0 l

-- | Create an array from a list.
arrayFromList :: [a] -> Array a
arrayFromList l = arrayFromListN (length l) l

#if MIN_VERSION_base(4,7,0)
instance Exts.IsList (Array a) where
  type Item (Array a) = a
  fromListN = arrayFromListN
  fromList = arrayFromList
  toList = toList
#else
fromListN :: Int -> [a] -> Array a
fromListN = arrayFromListN

fromList :: [a] -> Array a
fromList = arrayFromList
#endif

instance Functor Array where
  fmap f a =
    createArray (sizeofArray a) (die "fmap" "impossible") $ \mb ->
      let go i | i == sizeofArray a
               = return ()
               | otherwise
               = do x <- indexArrayM a i
                    writeArray mb i (f x) >> go (i+1)
       in go 0
#if MIN_VERSION_base(4,8,0)
  e <$ a = createArray (sizeofArray a) e (\ !_ -> pure ())
#endif

instance Applicative Array where
  pure x = runArray $ newArray 1 x
  ab <*> a = createArray (szab*sza) (die "<*>" "impossible") $ \mb ->
    let go1 i = when (i < szab) $
            do
              f <- indexArrayM ab i
              go2 (i*sza) f 0
              go1 (i+1)
        go2 off f j = when (j < sza) $
            do
              x <- indexArrayM a j
              writeArray mb (off + j) (f x)
              go2 off f (j + 1)
    in go1 0
   where szab = sizeofArray ab ; sza = sizeofArray a
  a *> b = createArray (sza*szb) (die "*>" "impossible") $ \mb ->
    let go i | i < sza   = copyArray mb (i * szb) b 0 szb
             | otherwise = return ()
     in go 0
   where sza = sizeofArray a ; szb = sizeofArray b
  a <* b = createArray (sza*szb) (die "<*" "impossible") $ \ma ->
    let fill off i e | i < szb   = writeArray ma (off+i) e >> fill off (i+1) e
                     | otherwise = return ()
        go i | i < sza
             = do x <- indexArrayM a i
                  fill (i*szb) 0 x >> go (i+1)
             | otherwise = return ()
     in go 0
   where sza = sizeofArray a ; szb = sizeofArray b

instance Alternative Array where
  empty = emptyArray
  a1 <|> a2 = createArray (sza1 + sza2) (die "<|>" "impossible") $ \ma ->
    copyArray ma 0 a1 0 sza1 >> copyArray ma sza1 a2 0 sza2
   where sza1 = sizeofArray a1 ; sza2 = sizeofArray a2
  some a | sizeofArray a == 0 = emptyArray
         | otherwise = die "some" "infinite arrays are not well defined"
  many a | sizeofArray a == 0 = pure []
         | otherwise = die "many" "infinite arrays are not well defined"

data ArrayStack a
  = PushArray !(Array a) !(ArrayStack a)
  | EmptyStack
-- See the note in SmallArray about how we might improve this.

instance Monad Array where
  return = pure
  (>>) = (*>)

  ary >>= f = collect 0 EmptyStack (la-1)
   where
   la = sizeofArray ary
   collect sz stk i
     | i < 0 = createArray sz (die ">>=" "impossible") $ fill 0 stk
     | (# x #) <- indexArray## ary i
     , let sb = f x
           lsb = sizeofArray sb
       -- If we don't perform this check, we could end up allocating
       -- a stack full of empty arrays if someone is filtering most
       -- things out. So we refrain from pushing empty arrays.
     = if lsb == 0
       then collect sz stk (i - 1)
       else collect (sz + lsb) (PushArray sb stk) (i-1)

   fill _   EmptyStack         _   = return ()
   fill off (PushArray sb sbs) smb
     | let lsb = sizeofArray sb
     = copyArray smb off sb 0 (lsb)
         *> fill (off + lsb) sbs smb

#if !(MIN_VERSION_base(4,13,0))
  fail = Fail.fail
#endif

instance Fail.MonadFail Array where
  fail _ = empty

instance MonadPlus Array where
  mzero = empty
  mplus = (<|>)

zipW :: String -> (a -> b -> c) -> Array a -> Array b -> Array c
zipW s f aa ab = createArray mn (die s "impossible") $ \mc ->
  let go i | i < mn
           = do
               x <- indexArrayM aa i
               y <- indexArrayM ab i
               writeArray mc i (f x y)
               go (i+1)
           | otherwise = return ()
   in go 0
 where mn = sizeofArray aa `min` sizeofArray ab
{-# INLINE zipW #-}

#if MIN_VERSION_base(4,4,0)
instance MonadZip Array where
  mzip aa ab = zipW "mzip" (,) aa ab
  mzipWith f aa ab = zipW "mzipWith" f aa ab
  munzip aab = runST $ do
    let sz = sizeofArray aab
    ma <- newArray sz (die "munzip" "impossible")
    mb <- newArray sz (die "munzip" "impossible")
    let go i | i < sz = do
          (a, b) <- indexArrayM aab i
          writeArray ma i a
          writeArray mb i b
          go (i+1)
        go _ = return ()
    go 0
    (,) <$> unsafeFreezeArray ma <*> unsafeFreezeArray mb
#endif

instance MonadFix Array where
  mfix f = createArray (sizeofArray (f err))
                       (die "mfix" "impossible") $ flip fix 0 $
    \r !i !mary -> when (i < sz) $ do
                      writeArray mary i (fix (\xi -> f xi `indexArray` i))
                      r (i + 1) mary
    where
      sz = sizeofArray (f err)
      err = error "mfix for Data.Primitive.Array applied to strict function."

#if MIN_VERSION_base(4,9,0)
-- | @since 0.6.3.0
instance Semigroup (Array a) where
  (<>) = (<|>)
  sconcat = mconcat . F.toList
#endif

instance Monoid (Array a) where
  mempty = empty
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<|>)
#endif
  mconcat l = createArray sz (die "mconcat" "impossible") $ \ma ->
    let go !_  [    ] = return ()
        go off (a:as) =
          copyArray ma off a 0 (sizeofArray a) >> go (off + sizeofArray a) as
     in go 0 l
   where sz = sum . fmap sizeofArray $ l

arrayLiftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> Array a -> ShowS
arrayLiftShowsPrec elemShowsPrec elemListShowsPrec p a = showParen (p > 10) $
  showString "fromListN " . shows (sizeofArray a) . showString " "
    . listLiftShowsPrec elemShowsPrec elemListShowsPrec 11 (toList a)

-- this need to be included for older ghcs
listLiftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> [a] -> ShowS
listLiftShowsPrec _ sl _ = sl

instance Show a => Show (Array a) where
  showsPrec p a = arrayLiftShowsPrec showsPrec showList p a

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
-- | @since 0.6.4.0
instance Show1 Array where
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
  liftShowsPrec = arrayLiftShowsPrec
#else
  showsPrec1 = arrayLiftShowsPrec showsPrec showList
#endif
#endif

instance Read a => Read (Array a) where
  readPrec = arrayLiftReadPrec readPrec readListPrec

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
-- | @since 0.6.4.0
instance Read1 Array where
#if MIN_VERSION_base(4,10,0)
  liftReadPrec = arrayLiftReadPrec
#elif MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
  liftReadsPrec = arrayLiftReadsPrec
#else
  readsPrec1 = arrayLiftReadsPrec readsPrec readList
#endif
#endif

-- We're really forgiving here. We accept
-- "[1,2,3]", "fromList [1,2,3]", and "fromListN 3 [1,2,3]".
-- We consider fromListN with an invalid length to be an
-- error, rather than a parse failure, because doing otherwise
-- seems weird and likely to make debugging difficult.
arrayLiftReadPrec :: ReadPrec a -> ReadPrec [a] -> ReadPrec (Array a)
arrayLiftReadPrec _ read_list = parens $ prec app_prec $ RdPrc.lift skipSpaces >>
    ((fromList <$> read_list) RdPrc.+++
      do
        tag <- RdPrc.lift lexTag
        case tag of
          FromListTag -> fromList <$> read_list
          FromListNTag -> liftM2 fromListN readPrec read_list)
   where
     app_prec = 10

data Tag = FromListTag | FromListNTag

-- Why don't we just use lexP? The general problem with lexP is that
-- it doesn't always fail as fast as we might like. It will
-- happily read to the end of an absurdly long lexeme (e.g., a 200MB string
-- literal) before returning, at which point we'll immediately discard
-- the result because it's not an identifier. Doing the job ourselves, we
-- can see very quickly when we've run into a problem. We should also get
-- a slight efficiency boost by going through the string just once.
lexTag :: ReadP Tag
lexTag = do
  _ <- string "fromList"
  s <- look
  case s of
    'N':c:_
      | '0' <= c && c <= '9'
      -> fail "" -- We have fromListN3 or similar
      | otherwise -> FromListNTag <$ get -- Skip the 'N'
    _ -> return FromListTag

#if !MIN_VERSION_base(4,10,0)
arrayLiftReadsPrec :: (Int -> ReadS a) -> ReadS [a] -> Int -> ReadS (Array a)
arrayLiftReadsPrec reads_prec list_reads_prec = RdPrc.readPrec_to_S $
  arrayLiftReadPrec (RdPrc.readS_to_Prec reads_prec) (RdPrc.readS_to_Prec (const list_reads_prec))
#endif


arrayDataType :: DataType
arrayDataType = mkDataType "Data.Primitive.Array.Array" [fromListConstr]

fromListConstr :: Constr
fromListConstr = mkConstr arrayDataType "fromList" [] Prefix

instance Data a => Data (Array a) where
  toConstr _ = fromListConstr
  dataTypeOf _ = arrayDataType
  gunfold k z c = case constrIndex c of
    1 -> k (z fromList)
    _ -> error "gunfold"
  gfoldl f z m = z fromList `f` toList m

instance (Typeable s, Typeable a) => Data (MutableArray s a) where
  toConstr _ = error "toConstr"
  gunfold _ _ = error "gunfold"
  dataTypeOf _ = mkNoRepType "Data.Primitive.Array.MutableArray"
