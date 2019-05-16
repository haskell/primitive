{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Apply
  (
#if defined(HAVE_SEMIGROUPOIDS) && defined(HAVE_UNARY_LAWS)
    applyLaws
#endif
) where

#if defined(HAVE_SEMIGROUPOIDS) && defined(HAVE_UNARY_LAWS)
import Data.Functor
import qualified Data.Functor.Apply as FunctorApply
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Arbitrary (Arbitrary1(..))
import Data.Functor.Classes (Eq1,Show1)
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common
import Test.QuickCheck.Classes.Compat (eq1)

-- | Tests the following alt properties:
--
-- [/LiftF2 (1)/]
--   @('FunctorApply.<.>') ≡ 'FunctorApply.liftF2' 'id'@
applyLaws ::
#if HAVE_QUANTIFIED_CONSTRAINTS
  (FunctorApply.Apply f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (FunctorApply.Apply f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Laws
applyLaws p = Laws "Apply"
  [ ("LiftF2 part 1", applyLiftF2_1 p)
  ]

applyLiftF2_1 :: forall proxy f. 
#if HAVE_QUANTIFIED_CONSTRAINTS
  (FunctorApply.Apply f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (FunctorApply.Apply f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
applyLiftF2_1 _ = property $ \(Apply (f' :: f QuadraticEquation)) (Apply (x :: f Integer)) ->
  let f = fmap runQuadraticEquation f'
  in eq1 (FunctorApply.liftF2 id f x) (f FunctorApply.<.> x)
#endif
