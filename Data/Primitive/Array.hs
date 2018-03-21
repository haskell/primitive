{-# LANGUAGE CPP, MagicHash, UnboxedTuples, DeriveDataTypeable, BangPatterns #-}
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

  newArray, readArray, writeArray, indexArray, indexArrayM,
  freezeArray, thawArray,
  unsafeFreezeArray, unsafeThawArray, sameMutableArray,
  copyArray, copyMutableArray,
  cloneArray, cloneMutableArray,
  sizeofArray, sizeofMutableArray,
  fromListN, fromList,
  unsafeTraverseArray
) where

import Control.Monad.Primitive

import GHC.Base  ( Int(..) )
import GHC.Prim
import qualified GHC.Exts as Exts
#if (MIN_VERSION_base(4,7,0))
import GHC.Exts (fromListN, fromList)
#endif

import Data.Typeable ( Typeable )
import Data.Data
  (Data(..), DataType, mkDataType, Constr, mkConstr, Fixity(..), constrIndex)
import Data.Primitive.Internal.Compat ( isTrue#, mkNoRepType )

import Control.Monad.ST(ST,runST)

import Control.Applicative
import Control.Monad (MonadPlus(..), when, unless)
import Control.Monad.Fix
#if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip
#endif
import Data.Foldable (Foldable(..), toList)
#if !(MIN_VERSION_base(4,8,0))
import Data.Traversable (Traversable(..))
import Data.Monoid
#endif
#if MIN_VERSION_base(4,9,0)
import qualified Data.Foldable as F
import Data.Semigroup
#endif
#if MIN_VERSION_base(4,8,0)
import Data.Functor.Identity
#endif
#if !(MIN_VERSION_base(4,8,0))
import Prelude hiding (foldr)
#endif

import Text.ParserCombinators.ReadP

-- | Boxed arrays
data Array a = Array
  { array# :: Array# a }
  deriving ( Typeable )

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
newArray :: PrimMonad m => Int -> a -> m (MutableArray (PrimState m) a)
{-# INLINE newArray #-}
newArray (I# n#) x = primitive
   (\s# -> case newArray# n# x s# of
             (# s'#, arr# #) ->
               let ma = MutableArray arr#
               in (# s'# , ma #))

-- | Read a value from the array at the given index.
readArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> m a
{-# INLINE readArray #-}
readArray arr (I# i#) = primitive (readArray# (marray# arr) i#)

-- | Write a value to the array at the given index.
writeArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> a -> m ()
{-# INLINE writeArray #-}
writeArray arr (I# i#) x = primitive_ (writeArray# (marray# arr) i# x)

-- | Read a value from the immutable array at the given index.
indexArray :: Array a -> Int -> a
{-# INLINE indexArray #-}
indexArray arr (I# i#) = case indexArray# (array# arr) i# of (# x #) -> x

-- | Read a value from the immutable array at the given index, returning
-- the result in an unboxed unary tuple. This is currently used to implement
-- folds.
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

-- | Copy a slice of a mutable array to another array. The two arrays may
-- not be the same.
copyMutableArray :: PrimMonad m
          => MutableArray (PrimState m) a    -- ^ destination array
          -> Int                             -- ^ offset into destination array
          -> MutableArray (PrimState m) a    -- ^ source array
          -> Int                             -- ^ offset into source array
          -> Int                             -- ^ number of elements to copy
          -> m ()
{-# INLINE copyMutableArray #-}
#if __GLASGOW_HASKELL__ >= 706
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
-- provided Array. The provided Array should contain the full subrange
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

createArray
  :: Int
  -> a
  -> (forall s. MutableArray s a -> ST s ())
  -> Array a
createArray 0 _ _ = emptyArray
createArray n x f = runST $ do
  ma <- newArray n x
  f ma
  unsafeFreezeArray ma

die :: String -> String -> a
die fun problem = error $ "Data.Primitive.Array." ++ fun ++ ": " ++ problem

instance Eq a => Eq (Array a) where
  a1 == a2 = sizeofArray a1 == sizeofArray a2 && loop (sizeofArray a1 - 1)
   where loop i | i < 0     = True
                | (# x1 #) <- indexArray## a1 i
                , (# x2 #) <- indexArray## a2 i
                = x1 == x2 && loop (i-1)

instance Eq (MutableArray s a) where
  ma1 == ma2 = isTrue# (sameMutableArray# (marray# ma1) (marray# ma2))

instance Ord a => Ord (Array a) where
  compare a1 a2 = loop 0
   where
   mn = sizeofArray a1 `min` sizeofArray a2
   loop i
     | i < mn
     , (# x1 #) <- indexArray## a1 i
     , (# x2 #) <- indexArray## a2 i
     = compare x1 x2 `mappend` loop (i+1)
     | otherwise = compare (sizeofArray a1) (sizeofArray a2)

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
                  | otherwise -> f x (go (i - 1))
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

{-# RULES
"traverse/ST" forall (f :: a -> ST s b). traverseArray f =
   unsafeTraverseArray f
"traverse/IO" forall (f :: a -> IO b). traverseArray f =
   unsafeTraverseArray f
 #-}
#if MIN_VERSION_base(4,8,0)
{-# RULES
"traverse/Id" forall (f :: a -> Identity b). traverseArray f =
   (coerce :: (Array a -> Array (Identity b))
           -> Array a -> Identity (Array b)) (fmap f)
 #-}
#endif

-- | This is the fastest, most straightforward way to traverse
-- an array, but it only works correctly with a sufficiently
-- "affine" 'PrimMonad' instance. In particular, it must only produce
-- *one* result array. 'Control.Monad.Trans.List.ListT'-transformed
-- monads, for example, will not work right at all.
unsafeTraverseArray
  :: PrimMonad m
  => (a -> m b)
  -> Array a
  -> m (Array b)
unsafeTraverseArray f = \ !ary ->
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
{-# INLINE unsafeTraverseArray #-}

#if MIN_VERSION_base(4,7,0)
instance Exts.IsList (Array a) where
  type Item (Array a) = a
  fromListN n l =
    createArray n fromListN_too_short $ \mi ->
      let
        go x r i
          | i < n = writeArray mi i x >> r (i + 1)
          | otherwise = fromListN_too_long
        stop i = unless (i == n) fromListN_too_short
      in foldr go stop l 0
  -- Ideally, we should probably start with an explicit recursive
  -- version and rewrite to and from a foldr-based one for fusion.
  -- That could reduce code size somewhat in the non-fusing case.
  {-# INLINE fromListN #-}

  -- Should fromList use array doubling and shrinking?
  fromList l = Exts.fromListN (length l) l
  toList = toList
#else
fromListN :: Int -> [a] -> Array a
fromListN n l =
  createArray n fromListN_too_short $ \mi ->
    let go i (x:xs)
          | i < n = writeArray mi i x >> go (i+1) xs
          | otherwise = die fromListN_too_long
        go i []
          | i == n = return ()
          | otherwise = fromListN_too_short
     in go 0 l

fromList :: [a] -> Array a
fromList l = fromListN (length l) l
#endif

fromListN_too_long, fromListN_too_short :: a
fromListN_too_long = die "fromListN" "list too long"
{-# NOINLINE fromListN_too_long #-}
fromListN_too_short = die "fromListN" "list too short"
{-# NOINLINE fromListN_too_short #-}

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
  e <$ a = runST $ newArray (sizeofArray a) e >>= unsafeFreezeArray
#endif

instance Applicative Array where
  pure x = runST $ newArray 1 x >>= unsafeFreezeArray
  ab <*> a = runST $ do
    mb <- newArray (szab*sza) $ die "<*>" "impossible"
    let go1 i = when (i < szab) $
            do
              f <- indexArrayM ab i
              go2 (i*sza) f 0
              go1 (i+1)
        go2 off f j = when (j < sza) $
            do
              x <- indexArrayM a j
              writeArray mb (off + j) (f x)
    go1 0
    unsafeFreezeArray mb
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

instance Show a => Show (Array a) where
  showsPrec p a = showParen (p > 10) $
    showString "fromListN " . shows (sizeofArray a) . showString " "
      . shows (toList a)

instance Read a => Read (Array a) where
  readsPrec p = readParen (p > 10) . readP_to_S $ do
    () <$ string "fromListN"
    skipSpaces
    n <- readS_to_P reads
    skipSpaces
    l <- readS_to_P reads
    return $ fromListN n l

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
