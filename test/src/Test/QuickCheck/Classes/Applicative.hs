{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Applicative
  (
#if HAVE_UNARY_LAWS
    applicativeLaws
#endif
  ) where

import Control.Applicative
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

-- | Tests the following applicative properties:
--
-- [/Identity/]
--   @'pure' 'id' '<*>' v ≡ v@
-- [/Composition/]
--   @'pure' ('.') '<*>' u '<*>' v '<*>' w ≡ u '<*>' (v '<*>' w)@
-- [/Homomorphism/]
--   @'pure' f '<*>' 'pure' x ≡ 'pure' (f x)@
-- [/Interchange/]
--   @u '<*>' 'pure' y ≡ 'pure' ('$' y) '<*>' u@
-- [/LiftA2 (1)/]
--   @('<*>') ≡ 'liftA2' 'id'@
applicativeLaws ::
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Applicative f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Applicative f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Laws
applicativeLaws p = Laws "Applicative"
  [ ("Identity", applicativeIdentity p)
  , ("Composition", applicativeComposition p)
  , ("Homomorphism", applicativeHomomorphism p)
  , ("Interchange", applicativeInterchange p)
  , ("LiftA2 Part 1", applicativeLiftA2_1 p)
    -- todo: liftA2 part 2, we need an equation of two variables for this
  ]

applicativeIdentity :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Applicative f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Applicative f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
applicativeIdentity _ = property $ \(Apply (a :: f Integer)) -> eq1 (pure id <*> a) a

applicativeComposition :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Applicative f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Applicative f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
applicativeComposition _ = property $ \(Apply (u' :: f QuadraticEquation)) (Apply (v' :: f QuadraticEquation)) (Apply (w :: f Integer)) ->
  let u = fmap runQuadraticEquation u'
      v = fmap runQuadraticEquation v'
   in eq1 (pure (.) <*> u <*> v <*> w) (u <*> (v <*> w))

applicativeHomomorphism :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Applicative f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a))
#else
  (Applicative f, Eq1 f, Show1 f)
#endif
  => proxy f -> Property
applicativeHomomorphism _ = property $ \(e :: QuadraticEquation) (a :: Integer) ->
  let f = runQuadraticEquation e
   in eq1 (pure f <*> pure a) (pure (f a) :: f Integer)

applicativeInterchange :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Applicative f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Applicative f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
applicativeInterchange _ = property $ \(Apply (u' :: f QuadraticEquation)) (y :: Integer) ->
  let u = fmap runQuadraticEquation u'
   in eq1 (u <*> pure y) (pure ($ y) <*> u)

applicativeLiftA2_1 :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Applicative f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Applicative f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
applicativeLiftA2_1 _ = property $ \(Apply (f' :: f QuadraticEquation)) (Apply (x :: f Integer)) ->
  let f = fmap runQuadraticEquation f'
   in eq1 (liftA2 id f x) (f <*> x)

#endif
