{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.MonadPlus
  (
#if HAVE_UNARY_LAWS
    monadPlusLaws
#endif
  ) where

import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property)
import Test.QuickCheck.Classes.Common
#if HAVE_UNARY_LAWS
import Test.QuickCheck.Classes.Compat (eq1)
#endif
import Control.Monad (MonadPlus(mzero,mplus))

#if HAVE_UNARY_LAWS
import Test.QuickCheck.Arbitrary (Arbitrary1(..))
import Data.Functor.Classes (Eq1,Show1)
#endif

#if HAVE_UNARY_LAWS

-- | Tests the following monad plus properties:
--
-- [/Left Identity/]
--   @'mplus' 'mzero' x ≡ x@
-- [/Right Identity/]
--   @'mplus' x 'mzero' ≡ x@
-- [/Associativity/]
--   @'mplus' a ('mplus' b c) ≡ 'mplus' ('mplus' a b) c)@ 
-- [/Left Zero/]
--   @'mzero' '>>=' f ≡ 'mzero'@
-- [/Right Zero/]
--   @m '>>' 'mzero' ≡ 'mzero'@
monadPlusLaws ::
#if HAVE_QUANTIFIED_CONSTRAINTS
  (MonadPlus f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (MonadPlus f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Laws
monadPlusLaws p = Laws "MonadPlus"
  [ ("Left Identity", monadPlusLeftIdentity p)
  , ("Right Identity", monadPlusRightIdentity p)
  , ("Associativity", monadPlusAssociativity p)
  , ("Left Zero", monadPlusLeftZero p)
  , ("Right Zero", monadPlusRightZero p)
  ]

monadPlusLeftIdentity :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (MonadPlus f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (MonadPlus f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
monadPlusLeftIdentity _ = property $ \(Apply (a :: f Integer)) -> eq1 (mplus mzero a) a

monadPlusRightIdentity :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (MonadPlus f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (MonadPlus f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
monadPlusRightIdentity _ = property $ \(Apply (a :: f Integer)) -> eq1 (mplus a mzero) a

monadPlusAssociativity :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (MonadPlus f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (MonadPlus f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
monadPlusAssociativity _ = property $ \(Apply (a :: f Integer)) (Apply (b :: f Integer)) (Apply (c :: f Integer)) -> eq1 (mplus a (mplus b c)) (mplus (mplus a b) c)

monadPlusLeftZero :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (MonadPlus f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (MonadPlus f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
monadPlusLeftZero _ = property $ \(k' :: LinearEquationM f) -> eq1 (mzero >>= runLinearEquationM k') mzero

monadPlusRightZero :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (MonadPlus f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (MonadPlus f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
monadPlusRightZero _ = property $ \(Apply (a :: f Integer)) -> eq1 (a >> (mzero :: f Integer)) mzero

#endif
