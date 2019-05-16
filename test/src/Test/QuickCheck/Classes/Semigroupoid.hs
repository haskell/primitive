{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Semigroupoid
  (
#if defined(HAVE_SEMIGROUPOIDS) && defined(HAVE_BINARY_LAWS)
    semigroupoidLaws
  , commutativeSemigroupoidLaws
#endif
  ) where

#if defined(HAVE_SEMIGROUPOIDS) && defined(HAVE_BINARY_LAWS)
import Prelude hiding (id, (.))
import Data.Semigroupoid (Semigroupoid(..))
import Test.QuickCheck hiding ((.&.))
import Data.Functor.Classes (Eq2,Show2)
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common
import Test.QuickCheck.Classes.Compat (eq2)

-- | Tests the following 'Semigroupoid' properties:
--
-- [/Associativity/]
--   @f `'o'` (g `'o'` h) ≡ (f `'o'` g) `'o'` h@
--
-- /Note/: This property test is only available when this package is built with
-- @base-4.9+@ or @transformers-0.5+@.
semigroupoidLaws :: forall proxy s.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Semigroupoid s, forall a b. (Eq a, Eq b) => Eq (s a b), forall a b. (Show a, Show b) => Show (s a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (s a b))
#else
  (Semigroupoid s, Eq2 s, Show2 s, Arbitrary2 s)
#endif
  => proxy s -> Laws
semigroupoidLaws p = Laws "Semigroupoid"
  [ ("Associativity", semigroupoidAssociativity p)
  ]

-- | Tests everything from 'semigroupoidLaws' plus the following:
--
-- [/Commutative/]
--   @f `'o'` g ≡ g `'o'` f@
--
-- /Note/: This property test is only available when this package is built with
-- @base-4.9+@ or @transformers-0.5+@.
commutativeSemigroupoidLaws :: forall proxy s.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Semigroupoid s, forall a b. (Eq a, Eq b) => Eq (s a b), forall a b. (Show a, Show b) => Show (s a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (s a b))
#else
  (Semigroupoid s, Eq2 s, Show2 s, Arbitrary2 s)
#endif
  => proxy s -> Laws
commutativeSemigroupoidLaws p = Laws "Commutative Semigroupoid" $ lawsProperties (semigroupoidLaws p) ++
  [ ("Commutative", semigroupoidCommutativity p)
  ]

semigroupoidAssociativity :: forall proxy s.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Semigroupoid s, forall a b. (Eq a, Eq b) => Eq (s a b), forall a b. (Show a, Show b) => Show (s a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (s a b))
#else
  (Semigroupoid s, Eq2 s, Show2 s, Arbitrary2 s)
#endif
  => proxy s -> Property
semigroupoidAssociativity _ = property $ \(Apply2 (f :: s Integer Integer)) (Apply2 (g :: s Integer Integer)) (Apply2 (h :: s Integer Integer)) -> eq2 (f `o` (g `o` h)) ((f `o` g) `o` h)

semigroupoidCommutativity :: forall proxy s.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Semigroupoid s, forall a b. (Eq a, Eq b) => Eq (s a b), forall a b. (Show a, Show b) => Show (s a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (s a b))
#else
  (Semigroupoid s, Eq2 s, Show2 s, Arbitrary2 s)
#endif
  => proxy s -> Property
semigroupoidCommutativity _ = property $ \(Apply2 (f :: s Integer Integer)) (Apply2 (g :: s Integer Integer)) -> eq2 (f `o` g) (g `o` f)

#endif
