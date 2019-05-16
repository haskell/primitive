{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Functor
  (
#if HAVE_UNARY_LAWS
    functorLaws
#endif
  ) where

import Data.Functor
import Test.QuickCheck hiding ((.&.))
#if HAVE_UNARY_LAWS
import Test.QuickCheck.Arbitrary (Arbitrary1(..))
import Data.Functor.Classes (Eq1,Show1)
#endif
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common
#if HAVE_UNARY_LAWS
import Test.QuickCheck.Classes.Compat (eq1)
#endif

#if HAVE_UNARY_LAWS

-- | Tests the following functor properties:
--
-- [/Identity/]
--   @'fmap' 'id' ≡ 'id'@
-- [/Composition/]
--   @'fmap' (f '.' g) ≡ 'fmap' f '.' 'fmap' g@
-- [/Const/]
--   @('<$') ≡ 'fmap' 'const'@
functorLaws ::
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Functor f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Functor f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f
  -> Laws
functorLaws p = Laws "Functor"
  [ ("Identity", functorIdentity p)
  , ("Composition", functorComposition p)
  , ("Const", functorConst p)
  ]

functorIdentity :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Functor f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Functor f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
functorIdentity _ = property $ \(Apply (a :: f Integer)) -> eq1 (fmap id a) a

functorComposition :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Functor f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Functor f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
functorComposition _ = property $ \(Apply (a :: f Integer)) ->
  eq1 (fmap func2 (fmap func1 a)) (fmap (func2 . func1) a)

functorConst :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Functor f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Functor f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
functorConst _ = property $ \(Apply (a :: f Integer)) ->
  eq1 (fmap (const 'X') a) ('X' <$ a)

#endif

