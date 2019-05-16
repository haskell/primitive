{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif
{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Generic
  (
#if MIN_VERSION_base(4,5,0)
    genericLaws
#if HAVE_UNARY_LAWS
  , generic1Laws
#endif
#endif
  ) where

#if MIN_VERSION_base(4,5,0)
import Control.Applicative
import Data.Semigroup as SG
import Data.Monoid as MD
import GHC.Generics
#if HAVE_UNARY_LAWS
import Data.Functor.Classes
#endif
import Data.Proxy (Proxy(Proxy))
import Test.QuickCheck
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common (Laws(..), Apply(..))

-- | Tests the following properties:
--
-- [/From-To Inverse/]
--   @'from' '.' 'to' ≡  'id'@
-- [/To-From Inverse/]
--   @'to' '.' 'from' ≡  'id'@
--
-- /Note:/ This property test is only available when
-- using @base-4.5@ or newer.
--
-- /Note:/ 'from' and 'to' don't actually care about
-- the type variable @x@ in @'Rep' a x@, so here we instantiate
-- it to @'()'@ by default. If you would like to instantiate @x@
-- as something else, please file a bug report.
genericLaws :: (Generic a, Eq a, Arbitrary a, Show a, Show (Rep a ()), Arbitrary (Rep a ()), Eq (Rep a ())) => Proxy a -> Laws
genericLaws pa = Laws "Generic"
  [ ("From-To inverse", fromToInverse pa (Proxy :: Proxy ()))
  , ("To-From inverse", toFromInverse pa)
  ]

toFromInverse :: forall proxy a. (Generic a, Eq a, Arbitrary a, Show a) => proxy a -> Property
toFromInverse _ = property $ \(v :: a) -> (to . from $ v) == v

fromToInverse ::
     forall proxy a x.
     (Generic a, Show (Rep a x), Arbitrary (Rep a x), Eq (Rep a x))
  => proxy a
  -> proxy x
  -> Property
fromToInverse _ _ = property $ \(r :: Rep a x) -> r == (from (to r :: a)) 

#if HAVE_UNARY_LAWS
-- | Tests the following properties:
--
-- [/From-To Inverse/]
--   @'from1' '.' 'to1' ≡  'id'@
-- [/To-From Inverse/]
--   @'to1' '.' 'from1' ≡  'id'@
--
-- /Note:/ This property test is only available when
-- using @base-4.9@ or newer.
generic1Laws :: (Generic1 f, Eq1 f, Arbitrary1 f, Show1 f, Eq1 (Rep1 f), Show1 (Rep1 f), Arbitrary1 (Rep1 f))
  => proxy f -> Laws
generic1Laws p = Laws "Generic1"
  [ ("From1-To1 inverse", fromToInverse1 p)
  , ("To1-From1 inverse", toFromInverse1 p)
  ]

-- hack for quantified constraints: under base >= 4.12,
-- our usual 'Apply' wrapper has Eq, Show, and Arbitrary
-- instances that are incompatible.
newtype GApply f a = GApply { getGApply :: f a }

instance (Applicative f, Semigroup a) => Semigroup (GApply f a) where
  GApply x <> GApply y = GApply $ liftA2 (SG.<>) x y

instance (Applicative f, Monoid a) => Monoid (GApply f a) where
  mempty = GApply $ pure mempty
  mappend (GApply x) (GApply y) = GApply $ liftA2 (MD.<>) x y

instance (Eq1 f, Eq a) => Eq (GApply f a) where
  GApply a == GApply b = eq1 a b

instance (Show1 f, Show a) => Show (GApply f a) where
  showsPrec p = showsPrec1 p . getGApply

instance (Arbitrary1 f, Arbitrary a) => Arbitrary (GApply f a) where
  arbitrary = fmap GApply arbitrary1
  shrink = map GApply . shrink1 . getGApply

toFromInverse1 :: forall proxy f. (Generic1 f, Eq1 f, Arbitrary1 f, Show1 f) => proxy f -> Property
toFromInverse1 _ = property $ \(GApply (v :: f Integer)) -> eq1 v (to1 . from1 $ v)

fromToInverse1 :: forall proxy f. (Generic1 f, Eq1 (Rep1 f), Arbitrary1 (Rep1 f), Show1 (Rep1 f)) => proxy f -> Property
fromToInverse1 _ = property $ \(GApply (r :: Rep1 f Integer)) -> eq1 r (from1 ((to1 $ r) :: f Integer))

#endif

#endif
