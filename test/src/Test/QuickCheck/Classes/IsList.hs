{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}

{-|

This module provides property tests for functions that operate on
list-like data types. If your data type is fully polymorphic in its
element type, is it recommended that you use @foldableLaws@ and
@traversableLaws@ from @Test.QuickCheck.Classes@. However, if your
list-like data type is either monomorphic in its element type
(like @Text@ or @ByteString@) or if it requires a typeclass
constraint on its element (like @Data.Vector.Unboxed@), the properties
provided here can be helpful for testing that your functions have
the expected behavior. All properties in this module require your data
type to have an 'IsList' instance.

-}
module Test.QuickCheck.Classes.IsList
  ( 
#if MIN_VERSION_base(4,7,0)
    isListLaws 
  , foldrProp
  , foldlProp
  , foldlMProp
  , mapProp
  , imapProp
  , imapMProp
  , traverseProp
  , generateProp
  , generateMProp
  , replicateProp
  , replicateMProp
  , filterProp
  , filterMProp
  , mapMaybeProp
  , mapMaybeMProp
#endif
  ) where

#if MIN_VERSION_base(4,7,0)
import Control.Applicative
import Control.Monad.ST (ST,runST)
import Control.Monad (mapM,filterM,replicateM)
import Control.Applicative (liftA2)
import GHC.Exts (IsList,Item,toList,fromList,fromListN)
import Data.Maybe (mapMaybe,catMaybes)
import Data.Proxy (Proxy)
import Data.Foldable (foldlM)
import Data.Traversable (traverse)
import Test.QuickCheck (Property,Arbitrary,CoArbitrary,(===),property,
  NonNegative(..))
#if MIN_VERSION_QuickCheck(2,10,0)
import Test.QuickCheck.Function (Function,Fun,applyFun,applyFun2)
#else
import Test.QuickCheck.Function (Function,Fun,apply)
#endif
import qualified Data.List as L

import Test.QuickCheck.Classes.Common (Laws(..), myForAllShrink)

-- | Tests the following properties:
--
-- [/Partial Isomorphism/]
--   @fromList . toList ≡ id@
-- [/Length Preservation/]
--   @fromList xs ≡ fromListN (length xs) xs@
--
-- /Note:/ This property test is only available when
-- using @base-4.7@ or newer.
isListLaws :: (IsList a, Show a, Show (Item a), Arbitrary a, Arbitrary (Item a), Eq a) => Proxy a -> Laws
isListLaws p = Laws "IsList"
  [ ("Partial Isomorphism", isListPartialIsomorphism p)
  , ("Length Preservation", isListLengthPreservation p)
  ]

isListPartialIsomorphism :: forall a. (IsList a, Show a, Arbitrary a, Eq a) => Proxy a -> Property
isListPartialIsomorphism _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "fromList (toList a)"
  (\a -> fromList (toList a))
  "a"
  (\a -> a)

isListLengthPreservation :: forall a. (IsList a, Show (Item a), Arbitrary (Item a), Eq a) => Proxy a -> Property
isListLengthPreservation _ = property $ \(xs :: [Item a]) ->
  (fromList xs :: a) == fromListN (length xs) xs

foldrProp :: (IsList c, Item c ~ a, Arbitrary c, Show c, Show a, CoArbitrary a, Function a)
  => Proxy a -- ^ input element type
  -> (forall b. (a -> b -> b) -> b -> c -> b) -- ^ foldr function
  -> Property
foldrProp _ f = property $ \c (b0 :: Integer) func ->
  let g = applyFun2 func in
  L.foldr g b0 (toList c) === f g b0 c
  
foldlProp :: (IsList c, Item c ~ a, Arbitrary c, Show c, Show a, CoArbitrary a, Function a)
  => Proxy a -- ^ input element type
  -> (forall b. (b -> a -> b) -> b -> c -> b) -- ^ foldl function
  -> Property
foldlProp _ f = property $ \c (b0 :: Integer) func ->
  let g = applyFun2 func in
  L.foldl g b0 (toList c) === f g b0 c

foldlMProp :: (IsList c, Item c ~ a, Arbitrary c, Show c, Show a, CoArbitrary a, Function a)
  => Proxy a -- ^ input element type
  -> (forall s b. (b -> a -> ST s b) -> b -> c -> ST s b) -- ^ monadic foldl function
  -> Property
foldlMProp _ f = property $ \c (b0 :: Integer) func ->
  runST (foldlM (stApplyFun2 func) b0 (toList c)) === runST (f (stApplyFun2 func) b0 c)

mapProp :: (IsList c, IsList d, Eq d, Show d, Show b, Item c ~ a, Item d ~ b, Arbitrary c, Arbitrary b, Show c, Show a, CoArbitrary a, Function a)
  => Proxy a -- ^ input element type
  -> Proxy b -- ^ output element type
  -> ((a -> b) -> c -> d) -- ^ map function
  -> Property
mapProp _ _ f = property $ \c func ->
  fromList (map (applyFun func) (toList c)) === f (applyFun func) c

imapProp :: (IsList c, IsList d, Eq d, Show d, Show b, Item c ~ a, Item d ~ b, Arbitrary c, Arbitrary b, Show c, Show a, CoArbitrary a, Function a)
  => Proxy a -- ^ input element type
  -> Proxy b -- ^ output element type
  -> ((Int -> a -> b) -> c -> d) -- ^ indexed map function
  -> Property
imapProp _ _ f = property $ \c func ->
  fromList (imapList (applyFun2 func) (toList c)) === f (applyFun2 func) c

imapMProp :: (IsList c, IsList d, Eq d, Show d, Show b, Item c ~ a, Item d ~ b, Arbitrary c, Arbitrary b, Show c, Show a, CoArbitrary a, Function a)
  => Proxy a -- ^ input element type
  -> Proxy b -- ^ output element type
  -> (forall s. (Int -> a -> ST s b) -> c -> ST s d) -- ^ monadic indexed map function
  -> Property
imapMProp _ _ f = property $ \c func ->
  fromList (runST (imapMList (stApplyFun2 func) (toList c))) === runST (f (stApplyFun2 func) c)

traverseProp :: (IsList c, IsList d, Eq d, Show d, Show b, Item c ~ a, Item d ~ b, Arbitrary c, Arbitrary b, Show c, Show a, CoArbitrary a, Function a)
  => Proxy a -- ^ input element type
  -> Proxy b -- ^ output element type
  -> (forall s. (a -> ST s b) -> c -> ST s d) -- ^ traverse function
  -> Property
traverseProp _ _ f = property $ \c func ->
  fromList (runST (mapM (return . applyFun func) (toList c))) === runST (f (return . applyFun func) c)

-- | Property for the @generate@ function, which builds a container
--   of a given length by applying a function to each index.
generateProp :: (Item c ~ a, Eq c, Show c, IsList c, Arbitrary a, Show a)
  => Proxy a -- ^ input element type
  -> (Int -> (Int -> a) -> c) -- generate function
  -> Property
generateProp _ f = property $ \(NonNegative len) func ->
  fromList (generateList len (applyFun func)) === f len (applyFun func)

generateMProp :: (Item c ~ a, Eq c, Show c, IsList c, Arbitrary a, Show a)
  => Proxy a -- ^ input element type
  -> (forall s. Int -> (Int -> ST s a) -> ST s c) -- monadic generate function
  -> Property
generateMProp _ f = property $ \(NonNegative len) func ->
  fromList (runST (stGenerateList len (stApplyFun func))) === runST (f len (stApplyFun func))

replicateProp :: (Item c ~ a, Eq c, Show c, IsList c, Arbitrary a, Show a)
  => Proxy a -- ^ input element type
  -> (Int -> a -> c) -- replicate function
  -> Property
replicateProp _ f = property $ \(NonNegative len) a ->
  fromList (replicate len a) === f len a

replicateMProp :: (Item c ~ a, Eq c, Show c, IsList c, Arbitrary a, Show a)
  => Proxy a -- ^ input element type
  -> (forall s. Int -> ST s a -> ST s c) -- replicate function
  -> Property
replicateMProp _ f = property $ \(NonNegative len) a ->
  fromList (runST (replicateM len (return a))) === runST (f len (return a))

-- | Property for the @filter@ function, which keeps elements for which
-- the predicate holds true.
filterProp :: (IsList c, Item c ~ a, Arbitrary c, Show c, Show a, Eq c, CoArbitrary a, Function a)
  => Proxy a -- ^ element type
  -> ((a -> Bool) -> c -> c) -- ^ map function
  -> Property
filterProp _ f = property $ \c func ->
  fromList (filter (applyFun func) (toList c)) === f (applyFun func) c

-- | Property for the @filterM@ function, which keeps elements for which
-- the predicate holds true in an applicative context.
filterMProp :: (IsList c, Item c ~ a, Arbitrary c, Show c, Show a, Eq c, CoArbitrary a, Function a)
  => Proxy a -- ^ element type
  -> (forall s. (a -> ST s Bool) -> c -> ST s c) -- ^ traverse function
  -> Property
filterMProp _ f = property $ \c func ->
  fromList (runST (filterM (return . applyFun func) (toList c))) === runST (f (return . applyFun func) c)

-- | Property for the @mapMaybe@ function, which keeps elements for which
-- the predicate holds true.
mapMaybeProp :: (IsList c, Item c ~ a, Item d ~ b, Eq d, IsList d, Arbitrary b, Show d, Show b, Arbitrary c, Show c, Show a, Eq c, CoArbitrary a, Function a)
  => Proxy a -- ^ input element type
  -> Proxy b -- ^ output element type
  -> ((a -> Maybe b) -> c -> d) -- ^ map function
  -> Property
mapMaybeProp _ _ f = property $ \c func ->
  fromList (mapMaybe (applyFun func) (toList c)) === f (applyFun func) c

mapMaybeMProp :: (IsList c, IsList d, Eq d, Show d, Show b, Item c ~ a, Item d ~ b, Arbitrary c, Arbitrary b, Show c, Show a, CoArbitrary a, Function a)
  => Proxy a -- ^ input element type
  -> Proxy b -- ^ output element type
  -> (forall s. (a -> ST s (Maybe b)) -> c -> ST s d) -- ^ traverse function
  -> Property
mapMaybeMProp _ _ f = property $ \c func ->
  fromList (runST (mapMaybeMList (return . applyFun func) (toList c))) === runST (f (return . applyFun func) c)

imapList :: (Int -> a -> b) -> [a] -> [b]
imapList f xs = map (uncurry f) (zip (enumFrom 0) xs)

imapMList :: (Int -> a -> ST s b) -> [a] -> ST s [b]
imapMList f = go 0 where
  go !_ [] = return []
  go !ix (x : xs) = liftA2 (:) (f ix x) (go (ix + 1) xs)

mapMaybeMList :: Applicative f => (a -> f (Maybe b)) -> [a] -> f [b]
mapMaybeMList f = fmap catMaybes . traverse f

generateList :: Int -> (Int -> a) -> [a]
generateList len f = go 0 where
  go !ix = if ix < len
    then f ix : go (ix + 1)
    else []

stGenerateList :: Int -> (Int -> ST s a) -> ST s [a]
stGenerateList len f = go 0 where
  go !ix = if ix < len
    then liftA2 (:) (f ix) (go (ix + 1))
    else return []

stApplyFun :: Fun a b -> a -> ST s b
stApplyFun f a = return (applyFun f a)

stApplyFun2 :: Fun (a,b) c -> a -> b -> ST s c
stApplyFun2 f a b = return (applyFun2 f a b)

#if !MIN_VERSION_QuickCheck(2,10,0)
applyFun :: Fun a b -> (a -> b)
applyFun = apply

applyFun2 :: Fun (a, b) c -> (a -> b -> c)
applyFun2 = curry . apply
#endif
#endif
