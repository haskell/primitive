{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Alternative
  (
#if HAVE_UNARY_LAWS
    alternativeLaws
#endif
  ) where

import Control.Applicative (Alternative(..))
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

-- | Tests the following alternative properties:
--
-- [/Left Identity/]
--   @'empty' '<|>' x ≡ x@
-- [/Right Identity/]
--   @x '<|>' 'empty' ≡ x@
-- [/Associativity/]
--   @a '<|>' (b '<|>' c) ≡ (a '<|>' b) '<|>' c)@
alternativeLaws ::
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Alternative f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Alternative f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Laws
alternativeLaws p = Laws "Alternative"
  [ ("Left Identity", alternativeLeftIdentity p)
  , ("Right Identity", alternativeRightIdentity p)
  , ("Associativity", alternativeAssociativity p)
  ]

alternativeLeftIdentity :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Alternative f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Alternative f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
alternativeLeftIdentity _ = property $ \(Apply (a :: f Integer)) -> (eq1 (empty <|> a) a)

alternativeRightIdentity :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Alternative f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Alternative f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
alternativeRightIdentity _ = property $ \(Apply (a :: f Integer)) -> (eq1 a (empty <|> a))

alternativeAssociativity :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Alternative f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Alternative f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
alternativeAssociativity _ = property $ \(Apply (a :: f Integer)) (Apply (b :: f Integer)) (Apply (c :: f Integer)) -> eq1 (a <|> (b <|> c)) ((a <|> b) <|> c)

#endif
