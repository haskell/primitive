{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.MonadZip
  (
#if HAVE_UNARY_LAWS
    monadZipLaws
#endif
  ) where

import Control.Applicative
import Control.Arrow (Arrow(..))
import Control.Monad.Zip (MonadZip(mzip))
import Test.QuickCheck hiding ((.&.))
import Control.Monad (liftM)
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

-- | Tests the following monadic zipping properties:
--
-- [/Naturality/]
--   @'liftM' (f '***' g) ('mzip' ma mb) = 'mzip' ('liftM' f ma) ('liftM' g mb)@
--
-- In the laws above, the infix function @'***'@ refers to a typeclass
-- method of 'Arrow'.
monadZipLaws ::
#if HAVE_QUANTIFIED_CONSTRAINTS
  (MonadZip f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (MonadZip f, Applicative f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Laws
monadZipLaws p = Laws "MonadZip"
  [ ("Naturality", monadZipNaturality p)
  ]

monadZipNaturality :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (MonadZip f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (MonadZip f, Functor f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
monadZipNaturality _ = property $ \(f' :: LinearEquation) (g' :: LinearEquation) (Apply (ma :: f Integer)) (Apply (mb :: f Integer)) ->
  let f = runLinearEquation f'
      g = runLinearEquation g'
   in eq1 (liftM (f *** g) (mzip ma mb)) (mzip (liftM f ma) (liftM g mb))

#endif
