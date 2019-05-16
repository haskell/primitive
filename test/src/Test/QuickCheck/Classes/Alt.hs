{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Alt
  (
#if defined(HAVE_SEMIGROUPOIDS) && defined(HAVE_UNARY_LAWS)
    altLaws
#endif
) where

#if defined(HAVE_SEMIGROUPOIDS) && defined(HAVE_UNARY_LAWS)
import Data.Functor
import Data.Functor.Alt (Alt)
import qualified Data.Functor.Alt as Alt
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Arbitrary (Arbitrary1(..))
import Data.Functor.Classes (Eq1,Show1)
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common
import Test.QuickCheck.Classes.Compat (eq1)

-- | Tests the following alt properties:
--
-- [/Associativity/]
--   @(a 'Alt.<!>' b) 'Alt.<!>' c ≡ a 'Alt.<!>' (b 'Alt.<!>' c)@
-- [/Left Distributivity/]
--   @f '<$>' (a 'Alt.<!>' b) ≡ (f '<$>' a) 'Alt.<!>' (f '<$>' b)@
altLaws :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Alt f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Alt f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Laws
altLaws p = Laws "Alt"
  [ ("Associativity", altAssociative p)
  , ("Left Distributivity", altLeftDistributive p)
  ]

altAssociative :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Alt f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Alt f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
altAssociative _ = property $ \(Apply (a :: f Integer)) (Apply (b :: f Integer)) (Apply (c :: f Integer)) -> eq1 ((a Alt.<!> b) Alt.<!> c) (a Alt.<!> (b Alt.<!> c))

altLeftDistributive :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Alt f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Alt f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
altLeftDistributive _ = property $ \(Apply (a :: f Integer)) (Apply (b :: f Integer)) -> eq1 (id <$> (a Alt.<!> b)) ((id <$> a) Alt.<!> (id <$> b))
#endif
