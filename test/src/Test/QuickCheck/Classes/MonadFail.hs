{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.MonadFail
  (
#if HAVE_UNARY_LAWS
    monadFailLaws
#endif
  ) where

#if HAVE_UNARY_LAWS

import Control.Applicative
import Test.QuickCheck hiding ((.&.))
import Control.Monad (ap)
import Test.QuickCheck.Arbitrary (Arbitrary1(..))
import Data.Functor.Classes (Eq1,Show1)
import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common
import Test.QuickCheck.Classes.Compat (eq1)

-- | Tests the following 'MonadFail' properties:
-- 
-- [/Left Zero/]
-- @'fail' s '>>=' f ≡ 'fail' s@
monadFailLaws :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (MonadFail f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (MonadFail f, Applicative f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Laws
monadFailLaws p = Laws "Monad"
  [ ("Left Zero", monadFailLeftZero p)
  ]
 
monadFailLeftZero :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (MonadFail f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (MonadFail f, Functor f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
monadFailLeftZero _ = property $ \(k' :: LinearEquationM f) (s :: String) ->
  let k = runLinearEquationM k'
  in eq1 (fail s >>= k) (fail s)

#endif
