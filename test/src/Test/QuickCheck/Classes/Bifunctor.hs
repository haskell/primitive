{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Bifunctor
  (
#if HAVE_BINARY_LAWS
    bifunctorLaws
#endif
  ) where

import Data.Bifunctor(Bifunctor(..))
import Test.QuickCheck hiding ((.&.))
#if HAVE_BINARY_LAWS
import Data.Functor.Classes (Eq2,Show2)
#endif
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common
#if HAVE_BINARY_LAWS
import Test.QuickCheck.Classes.Compat (eq2)
#endif

#if HAVE_BINARY_LAWS

-- | Tests the following 'Bifunctor' properties:
--
-- [/Identity/]
--   @'bimap' 'id' 'id' ≡ 'id'@
-- [/First Identity/]
--   @'first' 'id' ≡ 'id'@
-- [/Second Identity/] 
--   @'second' 'id' ≡ 'id'@
-- [/Bifunctor Composition/]
--   @'bimap' f g ≡ 'first' f '.' 'second' g@ 
--
-- /Note/: This property test is only available when this package is built with
-- @base-4.9+@ or @transformers-0.5+@.
bifunctorLaws :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Bifunctor f, forall a b. (Eq a, Eq b) => Eq (f a b), forall a b. (Show a, Show b) => Show (f a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b))
#else
  (Bifunctor f, Eq2 f, Show2 f, Arbitrary2 f)
#endif
  => proxy f -> Laws
bifunctorLaws p = Laws "Bifunctor"
  [ ("Identity", bifunctorIdentity p)
  , ("First Identity", bifunctorFirstIdentity p)
  , ("Second Identity", bifunctorSecondIdentity p)
  , ("Bifunctor Composition", bifunctorComposition p)
  ]

bifunctorIdentity :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Bifunctor f, forall a b. (Eq a, Eq b) => Eq (f a b), forall a b. (Show a, Show b) => Show (f a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b))
#else
  (Bifunctor f, Eq2 f, Show2 f, Arbitrary2 f)
#endif
  => proxy f -> Property
bifunctorIdentity _ = property $ \(Apply2 (x :: f Integer Integer)) -> eq2 (bimap id id x) x

bifunctorFirstIdentity :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Bifunctor f, forall a b. (Eq a, Eq b) => Eq (f a b), forall a b. (Show a, Show b) => Show (f a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b))
#else
  (Bifunctor f, Eq2 f, Show2 f, Arbitrary2 f)
#endif
  => proxy f -> Property
bifunctorFirstIdentity _ = property $ \(Apply2 (x :: f Integer Integer)) -> eq2 (first id x) x

bifunctorSecondIdentity :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Bifunctor f, forall a b. (Eq a, Eq b) => Eq (f a b), forall a b. (Show a, Show b) => Show (f a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b))
#else
  (Bifunctor f, Eq2 f, Show2 f, Arbitrary2 f)
#endif
  => proxy f -> Property
bifunctorSecondIdentity _ = property $ \(Apply2 (x :: f Integer Integer)) -> eq2 (second id x) x

bifunctorComposition :: forall proxy f. 
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Bifunctor f, forall a b. (Eq a, Eq b) => Eq (f a b), forall a b. (Show a, Show b) => Show (f a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b))
#else
  (Bifunctor f, Eq2 f, Show2 f, Arbitrary2 f)
#endif
  => proxy f -> Property
bifunctorComposition _ = property $ \(Apply2 (z :: f Integer Integer)) -> eq2 (bimap id id z) ((first id . second id) z)

#endif
