{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- |
-- Module : Data.Primitive.SmallArray
-- Copyright: (c) 2015 Dan Doel
-- License: BSD3
--
-- Maintainer: libraries@haskell.org
-- Portability: non-portable
--
-- Small arrays are boxed (im)mutable arrays.
--
-- The underlying structure of the 'Data.Primitive.Array.Array' type contains a card table, allowing
-- segments of the array to be marked as having been mutated. This allows the
-- garbage collector to only re-traverse segments of the array that have been
-- marked during certain phases, rather than having to traverse the entire
-- array.
--
-- 'SmallArray' lacks this table. This means that it takes up less memory and
-- has slightly faster writes. It is also more efficient during garbage
-- collection so long as the card table would have a single entry covering the
-- entire array. These advantages make them suitable for use as arrays that are
-- known to be small.
--
-- The card size is 128, so for uses much larger than that,
-- 'Data.Primitive.Array.Array' would likely be superior.

module Data.Primitive.SmallArray
  ( SmallArray(..)
  , SmallMutableArray(..)
  , newSmallArray
  , readSmallArray
  , writeSmallArray
  , copySmallArray
  , copySmallMutableArray
  , indexSmallArray
  , indexSmallArrayM
  , indexSmallArray##
  , cloneSmallArray
  , cloneSmallMutableArray
  , freezeSmallArray
  , unsafeFreezeSmallArray
  , thawSmallArray
  , unsafeThawSmallArray
  , runSmallArray
  , createSmallArray
  , sizeofSmallArray
  , getSizeofSmallMutableArray
  , sizeofSmallMutableArray
#if MIN_VERSION_base(4,14,0)
  , shrinkSmallMutableArray
  , resizeSmallMutableArray
#endif
  , emptySmallArray
  , smallArrayFromList
  , smallArrayFromListN
  , mapSmallArray'
  , traverseSmallArrayP
  ) where

import GHC.Exts hiding (toList)
import qualified GHC.Exts

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Zip
import Data.Data
import Data.Foldable as Foldable
import Data.Functor.Identity
import Data.Primitive.Internal.Read (Tag(..),lexTag)
import Text.Read (Read (..), parens, prec)
import qualified GHC.ST as GHCST
import Data.Semigroup
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (ReadPrec)
import qualified Text.ParserCombinators.ReadPrec as RdPrc

import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), Read1(..))
import Language.Haskell.TH.Syntax (Lift(..))

data SmallArray a = SmallArray (SmallArray# a)
  deriving Typeable

#if MIN_VERSION_deepseq(1,4,3)
instance NFData1 SmallArray where
  liftRnf r = foldl' (\_ -> r) ()
#endif

instance NFData a => NFData (SmallArray a) where
  rnf = foldl' (\_ -> rnf) ()

data SmallMutableArray s a = SmallMutableArray (SmallMutableArray# s a)
  deriving Typeable

instance Lift a => Lift (SmallArray a) where
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped ary = case lst of
    [] -> [|| SmallArray (emptySmallArray# (##)) ||]
    [x] -> [|| pure $! x ||]
    x : xs -> [|| unsafeSmallArrayFromListN' len x xs ||]
#else
  lift ary = case lst of
    [] -> [| SmallArray (emptySmallArray# (##)) |]
    [x] -> [| pure $! x |]
    x : xs -> [| unsafeSmallArrayFromListN' len x xs |]
#endif
    where
      len = length ary
      lst = toList ary

-- | Strictly create an array from a nonempty list (represented as
-- a first element and a list of the rest) of a known length. If the length
-- of the list does not match the given length, this makes demons fly
-- out of your nose. We use it in the 'Lift' instance. If you edit the
-- splice and break it, you get to keep both pieces.
unsafeSmallArrayFromListN' :: Int -> a -> [a] -> SmallArray a
unsafeSmallArrayFromListN' n y ys =
  createSmallArray n y $ \sma ->
    let go !_ix [] = return ()
        go !ix (!x : xs) = do
            writeSmallArray sma ix x
            go (ix+1) xs
    in go 1 ys

-- | Create a new small mutable array.
--
-- /Note:/ this function does not check if the input is non-negative.
newSmallArray
  :: PrimMonad m
  => Int -- ^ size
  -> a   -- ^ initial contents
  -> m (SmallMutableArray (PrimState m) a)
newSmallArray (I# i#) x = primitive $ \s ->
  case newSmallArray# i# x s of
    (# s', sma# #) -> (# s', SmallMutableArray sma# #)
{-# INLINE newSmallArray #-}

-- | Read the element at a given index in a mutable array.
--
-- /Note:/ this function does not do bounds checking.
readSmallArray
  :: PrimMonad m
  => SmallMutableArray (PrimState m) a -- ^ array
  -> Int                               -- ^ index
  -> m a
readSmallArray (SmallMutableArray sma#) (I# i#) =
  primitive $ readSmallArray# sma# i#
{-# INLINE readSmallArray #-}

-- | Write an element at the given idex in a mutable array.
--
-- /Note:/ this function does not do bounds checking.
writeSmallArray
  :: PrimMonad m
  => SmallMutableArray (PrimState m) a -- ^ array
  -> Int                               -- ^ index
  -> a                                 -- ^ new element
  -> m ()
writeSmallArray (SmallMutableArray sma#) (I# i#) x =
  primitive_ $ writeSmallArray# sma# i# x
{-# INLINE writeSmallArray #-}

-- | Look up an element in an immutable array.
--
-- The purpose of returning a result using an applicative is to allow the caller to
-- avoid retaining references to the array. Evaluating the return value will
-- cause the array lookup to be performed, even though it may not require the
-- element of the array to be evaluated (which could throw an exception). For
-- instance:
--
-- > data Box a = Box a
-- > ...
-- >
-- > f sa = case indexSmallArrayM sa 0 of
-- >   Box x -> ...
--
-- @x@ is not a closure that references @sa@ as it would be if we instead
-- wrote:
--
-- > let x = indexSmallArray sa 0
--
-- It also does not prevent @sa@ from being garbage collected.
--
-- Note that 'Identity' is not adequate for this use, as it is a newtype, and
-- cannot be evaluated without evaluating the element.
--
-- /Note:/ this function does not do bounds checking.
indexSmallArrayM
  :: Applicative m
  => SmallArray a -- ^ array
  -> Int          -- ^ index
  -> m a
indexSmallArrayM (SmallArray sa#) (I# i#) =
  case indexSmallArray# sa# i# of
    (# x #) -> pure x
{-# INLINE indexSmallArrayM #-}

-- | Look up an element in an immutable array.
--
-- /Note:/ this function does not do bounds checking.
indexSmallArray
  :: SmallArray a -- ^ array
  -> Int          -- ^ index
  -> a
indexSmallArray sa i = runIdentity $ indexSmallArrayM sa i
{-# INLINE indexSmallArray #-}

-- | Read a value from the immutable array at the given index, returning
-- the result in an unboxed unary tuple. This is currently used to implement
-- folds.
--
-- /Note:/ this function does not do bounds checking.
indexSmallArray## :: SmallArray a -> Int -> (# a #)
indexSmallArray## (SmallArray ary) (I# i) = indexSmallArray# ary i
{-# INLINE indexSmallArray## #-}

-- | Create a copy of a slice of an immutable array.
--
-- /Note:/ The provided array should contain the full subrange
-- specified by the two Ints, but this is not checked.
cloneSmallArray
  :: SmallArray a -- ^ source
  -> Int          -- ^ offset
  -> Int          -- ^ length
  -> SmallArray a
cloneSmallArray (SmallArray sa#) (I# i#) (I# j#) =
  SmallArray (cloneSmallArray# sa# i# j#)
{-# INLINE cloneSmallArray #-}

-- | Create a copy of a slice of a mutable array.
--
-- /Note:/ The provided array should contain the full subrange
-- specified by the two Ints, but this is not checked.
cloneSmallMutableArray
  :: PrimMonad m
  => SmallMutableArray (PrimState m) a -- ^ source
  -> Int                               -- ^ offset
  -> Int                               -- ^ length
  -> m (SmallMutableArray (PrimState m) a)
cloneSmallMutableArray (SmallMutableArray sma#) (I# o#) (I# l#) =
  primitive $ \s -> case cloneSmallMutableArray# sma# o# l# s of
    (# s', smb# #) -> (# s', SmallMutableArray smb# #)
{-# INLINE cloneSmallMutableArray #-}

-- | Create an immutable array corresponding to a slice of a mutable array.
--
-- This operation copies the portion of the array to be frozen.
--
-- /Note:/ The provided array should contain the full subrange
-- specified by the two Ints, but this is not checked.
freezeSmallArray
  :: PrimMonad m
  => SmallMutableArray (PrimState m) a -- ^ source
  -> Int                               -- ^ offset
  -> Int                               -- ^ length
  -> m (SmallArray a)
freezeSmallArray (SmallMutableArray sma#) (I# i#) (I# j#) =
  primitive $ \s -> case freezeSmallArray# sma# i# j# s of
    (# s', sa# #) -> (# s', SmallArray sa# #)
{-# INLINE freezeSmallArray #-}

-- | Render a mutable array immutable.
--
-- This operation performs no copying, so care must be taken not to modify the
-- input array after freezing.
unsafeFreezeSmallArray
  :: PrimMonad m => SmallMutableArray (PrimState m) a -> m (SmallArray a)
unsafeFreezeSmallArray (SmallMutableArray sma#) =
  primitive $ \s -> case unsafeFreezeSmallArray# sma# s of
    (# s', sa# #) -> (# s', SmallArray sa# #)
{-# INLINE unsafeFreezeSmallArray #-}

-- | Create a mutable array corresponding to a slice of an immutable array.
--
-- This operation copies the portion of the array to be thawed.
--
-- /Note:/ The provided array should contain the full subrange
-- specified by the two Ints, but this is not checked.
thawSmallArray
  :: PrimMonad m
  => SmallArray a -- ^ source
  -> Int          -- ^ offset
  -> Int          -- ^ length
  -> m (SmallMutableArray (PrimState m) a)
thawSmallArray (SmallArray sa#) (I# o#) (I# l#) =
  primitive $ \s -> case thawSmallArray# sa# o# l# s of
    (# s', sma# #) -> (# s', SmallMutableArray sma# #)
{-# INLINE thawSmallArray #-}

-- | Render an immutable array mutable.
--
-- This operation performs no copying, so care must be taken with its use.
unsafeThawSmallArray
  :: PrimMonad m => SmallArray a -> m (SmallMutableArray (PrimState m) a)
unsafeThawSmallArray (SmallArray sa#) =
  primitive $ \s -> case unsafeThawSmallArray# sa# s of
    (# s', sma# #) -> (# s', SmallMutableArray sma# #)
{-# INLINE unsafeThawSmallArray #-}

-- | Copy a slice of an immutable array into a mutable array.
--
-- /Note:/ this function does not do bounds or overlap checking.
copySmallArray
  :: PrimMonad m
  => SmallMutableArray (PrimState m) a -- ^ destination
  -> Int                               -- ^ destination offset
  -> SmallArray a                      -- ^ source
  -> Int                               -- ^ source offset
  -> Int                               -- ^ length
  -> m ()
copySmallArray
  (SmallMutableArray dst#) (I# do#) (SmallArray src#) (I# so#) (I# l#) =
    primitive_ $ copySmallArray# src# so# dst# do# l#
{-# INLINE copySmallArray #-}

-- | Copy a slice of one mutable array into another.
--
-- /Note:/ this function does not do bounds or overlap checking.
copySmallMutableArray
  :: PrimMonad m
  => SmallMutableArray (PrimState m) a -- ^ destination
  -> Int                               -- ^ destination offset
  -> SmallMutableArray (PrimState m) a -- ^ source
  -> Int                               -- ^ source offset
  -> Int                               -- ^ length
  -> m ()
copySmallMutableArray
  (SmallMutableArray dst#) (I# do#)
  (SmallMutableArray src#) (I# so#)
  (I# l#) =
    primitive_ $ copySmallMutableArray# src# so# dst# do# l#
{-# INLINE copySmallMutableArray #-}

-- | The number of elements in an immutable array.
sizeofSmallArray :: SmallArray a -> Int
sizeofSmallArray (SmallArray sa#) = I# (sizeofSmallArray# sa#)
{-# INLINE sizeofSmallArray #-}

-- | Get the number of elements in a mutable array. Unlike
-- 'sizeofSmallMutableArray', this function will be sure to produce the correct
-- result if 'SmallMutableArray' has been shrunk in place. Consider the following:
--
-- @
-- do
--   sa <- 'newSmallArray' 10 x
--   print $ 'sizeofSmallMutableArray' sa
--   'shrinkSmallMutableArray' sa 5
--   print $ sizeofSmallMutableArray sa
-- @
--
-- The compiler is well within its rights to eliminate the second size check
-- and print @10@ twice. However, 'getSizeofSmallMutableArray' will check
-- the size each time it's /executed/ (not /evaluated/), so it won't have this
-- problem:
--
-- @
-- do
--   sa <- 'newSmallArray' 10 x
--   print =<< getSizeofSmallMutableArray sa
--   'shrinkSmallMutableArray' sa 5
--   print =<< getSizeofSmallMutableArray sa
-- @
--
-- will certainly print @10@ and then @5@.
getSizeofSmallMutableArray
  :: PrimMonad m
  => SmallMutableArray (PrimState m) a
  -> m Int
#if MIN_VERSION_base(4,14,0)
getSizeofSmallMutableArray (SmallMutableArray sa#) = primitive $ \s ->
  case getSizeofSmallMutableArray# sa# s of
    (# s', sz# #) -> (# s', I# sz# #)
#else
getSizeofSmallMutableArray sa = pure $! sizeofSmallMutableArray sa
#endif
{-# INLINE getSizeofSmallMutableArray #-}

-- | The number of elements in a mutable array. This should only be used
-- for arrays that are not shrunk in place.
--
-- This is deprecated and will be removed in a future release. Use
-- 'getSizeofSmallMutableArray' instead.
sizeofSmallMutableArray :: SmallMutableArray s a -> Int
sizeofSmallMutableArray (SmallMutableArray sa#) =
  I# (sizeofSmallMutableArray# sa#)
{-# DEPRECATED sizeofSmallMutableArray "use getSizeofSmallMutableArray instead" #-}
{-# INLINE sizeofSmallMutableArray #-}

-- | This is the fastest, most straightforward way to traverse
-- an array, but it only works correctly with a sufficiently
-- "affine" 'PrimMonad' instance. In particular, it must only produce
-- /one/ result array. 'Control.Monad.Trans.List.ListT'-transformed
-- monads, for example, will not work right at all.
traverseSmallArrayP
  :: PrimMonad m
  => (a -> m b)
  -> SmallArray a
  -> m (SmallArray b)
traverseSmallArrayP f = \ !ary ->
  let
    !sz = sizeofSmallArray ary
    go !i !mary
      | i == sz
      = unsafeFreezeSmallArray mary
      | otherwise
      = do
          a <- indexSmallArrayM ary i
          b <- f a
          writeSmallArray mary i b
          go (i + 1) mary
  in do
    mary <- newSmallArray sz badTraverseValue
    go 0 mary
{-# INLINE traverseSmallArrayP #-}

-- | Strict map over the elements of the array.
mapSmallArray' :: (a -> b) -> SmallArray a -> SmallArray b
mapSmallArray' f sa = createSmallArray (length sa) (die "mapSmallArray'" "impossible") $ \smb ->
  fix ? 0 $ \go i ->
    when (i < length sa) $ do
      x <- indexSmallArrayM sa i
      let !y = f x
      writeSmallArray smb i y *> go (i + 1)
{-# INLINE mapSmallArray' #-}

-- | Execute the monadic action and freeze the resulting array.
--
-- > runSmallArray m = runST $ m >>= unsafeFreezeSmallArray
runSmallArray
  :: (forall s. ST s (SmallMutableArray s a))
  -> SmallArray a
-- This low-level business is designed to work with GHC's worker-wrapper
-- transformation. A lot of the time, we don't actually need an Array
-- constructor. By putting it on the outside, and being careful about
-- how we special-case the empty array, we can make GHC smarter about this.
-- The only downside is that separately created 0-length arrays won't share
-- their Array constructors, although they'll share their underlying
-- Array#s.
runSmallArray m = SmallArray (runSmallArray# m)

runSmallArray#
  :: (forall s. ST s (SmallMutableArray s a))
  -> SmallArray# a
runSmallArray# m = case runRW# $ \s ->
  case unST m s of { (# s', SmallMutableArray mary# #) ->
  unsafeFreezeSmallArray# mary# s'} of (# _, ary# #) -> ary#

unST :: ST s a -> State# s -> (# State# s, a #)
unST (GHCST.ST f) = f

-- | Create an array of the given size with a default value,
-- apply the monadic function and freeze the result. If the
-- size is 0, return 'emptySmallArray' (rather than a new copy thereof).
--
-- > createSmallArray 0 _ _ = emptySmallArray
-- > createSmallArray n x f = runSmallArray $ do
-- >   mary <- newSmallArray n x
-- >   f mary
-- >   pure mary
createSmallArray
  :: Int
  -> a
  -> (forall s. SmallMutableArray s a -> ST s ())
  -> SmallArray a
-- See the comment on runSmallArray for why we use emptySmallArray#.
createSmallArray 0 _ _ = SmallArray (emptySmallArray# (# #))
createSmallArray n x f = runSmallArray $ do
  mary <- newSmallArray n x
  f mary
  pure mary

emptySmallArray# :: (# #) -> SmallArray# a
emptySmallArray# _ = case emptySmallArray of SmallArray ar -> ar
{-# NOINLINE emptySmallArray# #-}

die :: String -> String -> a
die fun problem = error $ "Data.Primitive.SmallArray." ++ fun ++ ": " ++ problem

-- | The empty 'SmallArray'.
emptySmallArray :: SmallArray a
emptySmallArray =
  runST $ newSmallArray 0 (die "emptySmallArray" "impossible")
            >>= unsafeFreezeSmallArray
{-# NOINLINE emptySmallArray #-}


infixl 1 ?
(?) :: (a -> b -> c) -> (b -> a -> c)
(?) = flip
{-# INLINE (?) #-}

noOp :: a -> ST s ()
noOp = const $ pure ()

smallArrayLiftEq :: (a -> b -> Bool) -> SmallArray a -> SmallArray b -> Bool
smallArrayLiftEq p sa1 sa2 = length sa1 == length sa2 && loop (length sa1 - 1)
  where
  loop i
    | i < 0
    = True
    | (# x #) <- indexSmallArray## sa1 i
    , (# y #) <- indexSmallArray## sa2 i
    = p x y && loop (i - 1)

-- | @since 0.6.4.0
instance Eq1 SmallArray where
  liftEq = smallArrayLiftEq

instance Eq a => Eq (SmallArray a) where
  sa1 == sa2 = smallArrayLiftEq (==) sa1 sa2

instance Eq (SmallMutableArray s a) where
  SmallMutableArray sma1# == SmallMutableArray sma2# =
    isTrue# (sameSmallMutableArray# sma1# sma2#)

smallArrayLiftCompare :: (a -> b -> Ordering) -> SmallArray a -> SmallArray b -> Ordering
smallArrayLiftCompare elemCompare a1 a2 = loop 0
  where
  mn = length a1 `min` length a2
  loop i
    | i < mn
    , (# x1 #) <- indexSmallArray## a1 i
    , (# x2 #) <- indexSmallArray## a2 i
    = elemCompare x1 x2 `mappend` loop (i + 1)
    | otherwise = compare (length a1) (length a2)

-- | @since 0.6.4.0
instance Ord1 SmallArray where
  liftCompare = smallArrayLiftCompare

-- | Lexicographic ordering. Subject to change between major versions.
instance Ord a => Ord (SmallArray a) where
  compare sa1 sa2 = smallArrayLiftCompare compare sa1 sa2

instance Foldable SmallArray where
  -- Note: we perform the array lookups eagerly so we won't
  -- create thunks to perform lookups even if GHC can't see
  -- that the folding function is strict.
  foldr f = \z !ary ->
    let
      !sz = sizeofSmallArray ary
      go i
        | i == sz = z
        | (# x #) <- indexSmallArray## ary i
        = f x (go (i + 1))
    in go 0
  {-# INLINE foldr #-}
  foldl f = \z !ary ->
    let
      go i
        | i < 0 = z
        | (# x #) <- indexSmallArray## ary i
        = f (go (i - 1)) x
    in go (sizeofSmallArray ary - 1)
  {-# INLINE foldl #-}
  foldr1 f = \ !ary ->
    let
      !sz = sizeofSmallArray ary - 1
      go i =
        case indexSmallArray## ary i of
          (# x #) | i == sz -> x
                  | otherwise -> f x (go (i + 1))
    in if sz < 0
       then die "foldr1" "Empty SmallArray"
       else go 0
  {-# INLINE foldr1 #-}
  foldl1 f = \ !ary ->
    let
      !sz = sizeofSmallArray ary - 1
      go i =
        case indexSmallArray## ary i of
          (# x #) | i == 0 -> x
                  | otherwise -> f (go (i - 1)) x
    in if sz < 0
       then die "foldl1" "Empty SmallArray"
       else go sz
  {-# INLINE foldl1 #-}
  foldr' f = \z !ary ->
    let
      go i !acc
        | i == -1 = acc
        | (# x #) <- indexSmallArray## ary i
        = go (i - 1) (f x acc)
    in go (sizeofSmallArray ary - 1) z
  {-# INLINE foldr' #-}
  foldl' f = \z !ary ->
    let
      !sz = sizeofSmallArray ary
      go i !acc
        | i == sz = acc
        | (# x #) <- indexSmallArray## ary i
        = go (i + 1) (f acc x)
    in go 0 z
  {-# INLINE foldl' #-}
  null a = sizeofSmallArray a == 0
  {-# INLINE null #-}
  length = sizeofSmallArray
  {-# INLINE length #-}
  maximum ary | sz == 0   = die "maximum" "Empty SmallArray"
              | (# frst #) <- indexSmallArray## ary 0
              = go 1 frst
   where
     sz = sizeofSmallArray ary
     go i !e
       | i == sz = e
       | (# x #) <- indexSmallArray## ary i
       = go (i + 1) (max e x)
  {-# INLINE maximum #-}
  minimum ary | sz == 0   = die "minimum" "Empty SmallArray"
              | (# frst #) <- indexSmallArray## ary 0
              = go 1 frst
   where sz = sizeofSmallArray ary
         go i !e
           | i == sz = e
           | (# x #) <- indexSmallArray## ary i
           = go (i + 1) (min e x)
  {-# INLINE minimum #-}
  sum = foldl' (+) 0
  {-# INLINE sum #-}
  product = foldl' (*) 1
  {-# INLINE product #-}

newtype STA a = STA { _runSTA :: forall s. SmallMutableArray# s a -> ST s (SmallArray a) }

runSTA :: Int -> STA a -> SmallArray a
runSTA !sz = \ (STA m) -> runST $ newSmallArray_ sz >>=
                        \ (SmallMutableArray ar#) -> m ar#
{-# INLINE runSTA #-}

newSmallArray_ :: Int -> ST s (SmallMutableArray s a)
newSmallArray_ !n = newSmallArray n badTraverseValue

badTraverseValue :: a
badTraverseValue = die "traverse" "bad indexing"
{-# NOINLINE badTraverseValue #-}

instance Traversable SmallArray where
  traverse f = traverseSmallArray f
  {-# INLINE traverse #-}

traverseSmallArray
  :: Applicative f
  => (a -> f b) -> SmallArray a -> f (SmallArray b)
traverseSmallArray f = \ !ary ->
  let
    !len = sizeofSmallArray ary
    go !i
      | i == len
      = pure $ STA $ \mary -> unsafeFreezeSmallArray (SmallMutableArray mary)
      | (# x #) <- indexSmallArray## ary i
      = liftA2 (\b (STA m) -> STA $ \mary ->
                  writeSmallArray (SmallMutableArray mary) i b >> m mary)
               (f x) (go (i + 1))
  in if len == 0
    then pure emptySmallArray
    else runSTA len <$> go 0
{-# INLINE [1] traverseSmallArray #-}

{-# RULES
"traverse/ST" forall (f :: a -> ST s b). traverseSmallArray f = traverseSmallArrayP f
"traverse/IO" forall (f :: a -> IO b). traverseSmallArray f = traverseSmallArrayP f
"traverse/Id" forall (f :: a -> Identity b). traverseSmallArray f =
   (coerce :: (SmallArray a -> SmallArray (Identity b))
           -> SmallArray a -> Identity (SmallArray b)) (fmap f)
 #-}


instance Functor SmallArray where
  fmap f sa = createSmallArray (length sa) (die "fmap" "impossible") $ \smb ->
    fix ? 0 $ \go i ->
      when (i < length sa) $ do
        x <- indexSmallArrayM sa i
        writeSmallArray smb i (f x) *> go (i + 1)
  {-# INLINE fmap #-}

  x <$ sa = createSmallArray (length sa) x noOp

instance Applicative SmallArray where
  pure x = createSmallArray 1 x noOp

  sa *> sb = createSmallArray (la * lb) (die "*>" "impossible") $ \smb ->
    fix ? 0 $ \go i ->
      when (i < la) $
        copySmallArray smb (i * lb) sb 0 lb *> go (i + 1)
   where
    la = length sa; lb = length sb

  a <* b = createSmallArray (sza * szb) (die "<*" "impossible") $ \ma ->
    let fill off i e = when (i < szb) $
                         writeSmallArray ma (off + i) e >> fill off (i + 1) e
        go i = when (i < sza) $ do
                 x <- indexSmallArrayM a i
                 fill (i * szb) 0 x
                 go (i + 1)
     in go 0
   where sza = sizeofSmallArray a; szb = sizeofSmallArray b

  ab <*> a = createSmallArray (szab * sza) (die "<*>" "impossible") $ \mb ->
    let go1 i = when (i < szab) $
            do
              f <- indexSmallArrayM ab i
              go2 (i * sza) f 0
              go1 (i + 1)
        go2 off f j = when (j < sza) $
            do
              x <- indexSmallArrayM a j
              writeSmallArray mb (off + j) (f x)
              go2 off f (j + 1)
    in go1 0
   where szab = sizeofSmallArray ab; sza = sizeofSmallArray a

instance Alternative SmallArray where
  empty = emptySmallArray

  sl <|> sr =
    createSmallArray (length sl + length sr) (die "<|>" "impossible") $ \sma ->
      copySmallArray sma 0 sl 0 (length sl)
        *> copySmallArray sma (length sl) sr 0 (length sr)

  many sa | null sa   = pure []
          | otherwise = die "many" "infinite arrays are not well defined"

  some sa | null sa   = emptySmallArray
          | otherwise = die "some" "infinite arrays are not well defined"

data ArrayStack a
  = PushArray !(SmallArray a) !(ArrayStack a)
  | EmptyStack
-- TODO: This isn't terribly efficient. It would be better to wrap
-- ArrayStack with a type like
--
-- data NES s a = NES !Int !(SmallMutableArray s a) !(ArrayStack a)
--
-- We'd copy incoming arrays into the mutable array until we would
-- overflow it. Then we'd freeze it, push it on the stack, and continue.
-- Any sufficiently large incoming arrays would go straight on the stack.
-- Such a scheme would make the stack much more compact in the case
-- of many small arrays.

instance Monad SmallArray where
  return = pure
  (>>) = (*>)

  sa >>= f = collect 0 EmptyStack (la - 1)
   where
    la = length sa
    collect sz stk i
      | i < 0 = createSmallArray sz (die ">>=" "impossible") $ fill 0 stk
      | (# x #) <- indexSmallArray## sa i
      , let sb = f x
            lsb = length sb
        -- If we don't perform this check, we could end up allocating
        -- a stack full of empty arrays if someone is filtering most
        -- things out. So we refrain from pushing empty arrays.
      = if lsb == 0
        then collect sz stk (i - 1)
        else collect (sz + lsb) (PushArray sb stk) (i - 1)

    fill _ EmptyStack _ = return ()
    fill off (PushArray sb sbs) smb =
      copySmallArray smb off sb 0 (length sb)
        *> fill (off + length sb) sbs smb

#if !(MIN_VERSION_base(4,13,0))
  fail = Fail.fail
#endif

instance Fail.MonadFail SmallArray where
  fail _ = emptySmallArray

instance MonadPlus SmallArray where
  mzero = empty
  mplus = (<|>)

zipW :: String -> (a -> b -> c) -> SmallArray a -> SmallArray b -> SmallArray c
zipW nm = \f sa sb -> let mn = length sa `min` length sb in
  createSmallArray mn (die nm "impossible") $ \mc ->
    fix ? 0 $ \go i -> when (i < mn) $ do
      x <- indexSmallArrayM sa i
      y <- indexSmallArrayM sb i
      writeSmallArray mc i (f x y)
      go (i + 1)
{-# INLINE zipW #-}

instance MonadZip SmallArray where
  mzip = zipW "mzip" (,)
  mzipWith = zipW "mzipWith"
  {-# INLINE mzipWith #-}
  munzip sab = runST $ do
    let sz = length sab
    sma <- newSmallArray sz $ die "munzip" "impossible"
    smb <- newSmallArray sz $ die "munzip" "impossible"
    fix ? 0 $ \go i ->
      when (i < sz) $ case indexSmallArray sab i of
        (x, y) -> do writeSmallArray sma i x
                     writeSmallArray smb i y
                     go (i + 1)
    (,) <$> unsafeFreezeSmallArray sma
        <*> unsafeFreezeSmallArray smb

instance MonadFix SmallArray where
  mfix f = createSmallArray (sizeofSmallArray (f err))
                            (die "mfix" "impossible") $ fix ? 0 $
    \r !i !mary -> when (i < sz) $ do
                      writeSmallArray mary i (fix (\xi -> f xi `indexSmallArray` i))
                      r (i + 1) mary
    where
      sz = sizeofSmallArray (f err)
      err = error "mfix for Data.Primitive.SmallArray applied to strict function."

-- | @since 0.6.3.0
instance Semigroup (SmallArray a) where
  (<>) = (<|>)
  sconcat = mconcat . toList
  stimes n arr = case compare n 0 of
    LT -> die "stimes" "negative multiplier"
    EQ -> empty
    GT -> createSmallArray (n' * sizeofSmallArray arr) (die "stimes" "impossible") $ \sma ->
      let go i = when (i < n') $ do
            copySmallArray sma (i * sizeofSmallArray arr) arr 0 (sizeofSmallArray arr)
            go (i + 1)
      in go 0
    where n' = fromIntegral n :: Int

instance Monoid (SmallArray a) where
  mempty = empty
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif
  mconcat l = createSmallArray n (die "mconcat" "impossible") $ \ma ->
    let go !_  [    ] = return ()
        go off (a:as) =
          copySmallArray ma off a 0 (sizeofSmallArray a) >> go (off + sizeofSmallArray a) as
     in go 0 l
   where n = sum (fmap length l)

instance IsList (SmallArray a) where
  type Item (SmallArray a) = a
  fromListN = smallArrayFromListN
  fromList = smallArrayFromList
  toList = Foldable.toList

smallArrayLiftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> SmallArray a -> ShowS
smallArrayLiftShowsPrec elemShowsPrec elemListShowsPrec _ sa =
  listLiftShowsPrec elemShowsPrec elemListShowsPrec 11 (toList sa)

-- this need to be included for older ghcs
listLiftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> [a] -> ShowS
listLiftShowsPrec _ sl _ = sl

instance Show a => Show (SmallArray a) where
  showsPrec p sa = smallArrayLiftShowsPrec showsPrec showList p sa

-- | @since 0.6.4.0
instance Show1 SmallArray where
  liftShowsPrec = smallArrayLiftShowsPrec

-- See Note [Forgiving Array Read Instance]
smallArrayLiftReadPrec :: ReadPrec a -> ReadPrec [a] -> ReadPrec (SmallArray a)
smallArrayLiftReadPrec _ read_list =
  ( RdPrc.lift skipSpaces >> fmap fromList read_list )
  RdPrc.+++
  ( parens $ prec app_prec $ do
      RdPrc.lift skipSpaces
      tag <- RdPrc.lift lexTag
      case tag of
        FromListTag -> fromList <$> read_list
        FromListNTag -> liftM2 fromListN readPrec read_list
  )
  where
  app_prec = 10

instance Read a => Read (SmallArray a) where
  readPrec = smallArrayLiftReadPrec readPrec readListPrec

-- | @since 0.6.4.0
instance Read1 SmallArray where
  liftReadPrec = smallArrayLiftReadPrec

smallArrayDataType :: DataType
smallArrayDataType =
  mkDataType "Data.Primitive.SmallArray.SmallArray" [fromListConstr]

fromListConstr :: Constr
fromListConstr = mkConstr smallArrayDataType "fromList" [] Prefix

instance Data a => Data (SmallArray a) where
  toConstr _ = fromListConstr
  dataTypeOf _ = smallArrayDataType
  gunfold k z c = case constrIndex c of
    1 -> k (z fromList)
    _ -> die "gunfold" "SmallArray"
  gfoldl f z m = z fromList `f` toList m

instance (Typeable s, Typeable a) => Data (SmallMutableArray s a) where
  toConstr _ = die "toConstr" "SmallMutableArray"
  gunfold _ _ = die "gunfold" "SmallMutableArray"
  dataTypeOf _ = mkNoRepType "Data.Primitive.SmallArray.SmallMutableArray"

-- | Create a 'SmallArray' from a list of a known length. If the length
-- of the list does not match the given length, this throws an exception.
smallArrayFromListN :: Int -> [a] -> SmallArray a
{-# INLINE smallArrayFromListN #-}
smallArrayFromListN n l =
  createSmallArray n
      (die "smallArrayFromListN" "uninitialized element") $ \sma ->
  let z ix# = if I# ix# == n
        then return ()
        else die "smallArrayFromListN" "list length less than specified size"
      f x k = GHC.Exts.oneShot $ \ix# -> if I# ix# < n
        then do
          writeSmallArray sma (I# ix#) x
          k (ix# +# 1#)
        else die "smallArrayFromListN" "list length greater than specified size"
  in foldr f z l 0#

-- | Create a 'SmallArray' from a list.
smallArrayFromList :: [a] -> SmallArray a
smallArrayFromList l = smallArrayFromListN (length l) l

#if MIN_VERSION_base(4,14,0)
-- | Shrink the mutable array in place. The size given must be equal to
-- or less than the current size of the array. This is not checked.
shrinkSmallMutableArray :: PrimMonad m
  => SmallMutableArray (PrimState m) a
  -> Int
  -> m ()
{-# inline shrinkSmallMutableArray #-}
shrinkSmallMutableArray (SmallMutableArray x) (I# n) = primitive
  (\s0 -> case GHC.Exts.shrinkSmallMutableArray# x n s0 of
    s1 -> (# s1, () #)
  )

-- | Resize a mutable array to new specified size. The returned
-- 'SmallMutableArray' is either the original 'SmallMutableArray'
-- resized in-place or, if not possible, a newly allocated
-- 'SmallMutableArray' with the original content copied over.
--
-- To avoid undefined behaviour, the original 'SmallMutableArray'
-- shall not be accessed anymore after a 'resizeSmallMutableArray' has
-- been performed. Moreover, no reference to the old one should be
-- kept in order to allow garbage collection of the original
-- 'SmallMutableArray' in case a new 'SmallMutableArray' had to be
-- allocated.
resizeSmallMutableArray :: PrimMonad m
  => SmallMutableArray (PrimState m) a
  -> Int -- ^ New size
  -> a   -- ^ Newly created slots initialized to this element. Only used when array is grown.
  -> m (SmallMutableArray (PrimState m) a)
resizeSmallMutableArray (SmallMutableArray arr) (I# n) x = primitive
  (\s0 -> case GHC.Exts.resizeSmallMutableArray# arr n x s0 of
    (# s1, arr' #) -> (# s1, SmallMutableArray arr' #)
  )
{-# INLINE resizeSmallMutableArray #-}
#endif
