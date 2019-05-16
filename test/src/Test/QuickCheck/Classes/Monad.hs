{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Monad
  (
#if HAVE_UNARY_LAWS
    monadLaws
#endif
  ) where

import Control.Applicative
import Test.QuickCheck hiding ((.&.))
import Control.Monad (ap)
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

-- | Tests the following monadic properties:
--
-- [/Left Identity/]
--   @'return' a '>>=' k ≡ k a@
-- [/Right Identity/]
--   @m '>>=' 'return' ≡ m@
-- [/Associativity/]
--   @m '>>=' (\\x -> k x '>>=' h) ≡ (m '>>=' k) '>>=' h@
-- [/Return/]
--   @'pure' ≡ 'return'@
-- [/Ap/]
--   @('<*>') ≡ 'ap'@
monadLaws ::
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Monad f, Applicative f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Monad f, Applicative f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Laws
monadLaws p = Laws "Monad"
  [ ("Left Identity", monadLeftIdentity p)
  , ("Right Identity", monadRightIdentity p)
  , ("Associativity", monadAssociativity p)
  , ("Return", monadReturn p)
  , ("Ap", monadAp p)
  ]

monadLeftIdentity :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Monad f, Functor f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Monad f, Functor f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
monadLeftIdentity _ = property $ \(k' :: LinearEquationM f) (a :: Integer) ->
  let k = runLinearEquationM k'
   in eq1 (return a >>= k) (k a)

monadRightIdentity :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Monad f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Monad f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
monadRightIdentity _ = property $ \(Apply (m :: f Integer)) ->
  eq1 (m >>= return) m

monadAssociativity :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Monad f, Functor f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Monad f, Functor f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
monadAssociativity _ = property $ \(Apply (m :: f Integer)) (k' :: LinearEquationM f) (h' :: LinearEquationM f) ->
  let k = runLinearEquationM k'
      h = runLinearEquationM h'
   in eq1 (m >>= (\x -> k x >>= h)) ((m >>= k) >>= h)

monadReturn :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Monad f, Applicative f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Monad f, Applicative f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
monadReturn _ = property $ \(x :: Integer) ->
  eq1 (return x) (pure x :: f Integer)

monadAp :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Monad f, Applicative f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Monad f, Applicative f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
monadAp _ = property $ \(Apply (f' :: f QuadraticEquation)) (Apply (x :: f Integer)) ->
  let f = fmap runQuadraticEquation f'
   in eq1 (ap f x) (f <*> x)

#endif
