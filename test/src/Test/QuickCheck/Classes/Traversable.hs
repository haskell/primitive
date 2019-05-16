{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Traversable
  (
#if HAVE_UNARY_LAWS
    traversableLaws
#endif
  ) where

import Data.Foldable (foldMap)
import Data.Traversable (Traversable,fmapDefault,foldMapDefault,sequenceA,traverse)
import Test.QuickCheck hiding ((.&.))
#if HAVE_UNARY_LAWS
import Test.QuickCheck.Arbitrary (Arbitrary1(..))
import Data.Functor.Classes (Eq1,Show1)
#endif
import Data.Functor.Compose
import Data.Functor.Identity

import Test.QuickCheck.Classes.Common
#if HAVE_UNARY_LAWS
import Test.QuickCheck.Classes.Compat (eq1)
#endif

#if HAVE_UNARY_LAWS

-- | Tests the following 'Traversable' properties:
--
-- [/Naturality/]
--   @t '.' 'traverse' f ≡ 'traverse' (t '.' f)@
--   for every applicative transformation @t@
-- [/Identity/]
--   @'traverse' 'Identity' ≡ 'Identity'@
-- [/Composition/]
--   @'traverse' ('Compose' '.' 'fmap' g '.' f) ≡ 'Compose' '.' 'fmap' ('traverse' g) '.' 'traverse' f@
-- [/Sequence Naturality/]
--   @t '.' 'sequenceA' ≡ 'sequenceA' '.' 'fmap' t@
--   for every applicative transformation @t@
-- [/Sequence Identity/]
--   @'sequenceA' '.' 'fmap' 'Identity' ≡ 'Identity'@
-- [/Sequence Composition/]
--   @'sequenceA' '.' 'fmap' 'Compose' ≡ 'Compose' '.' 'fmap' 'sequenceA' '.' 'sequenceA'@
-- [/foldMap/]
--   @'foldMap' ≡ 'foldMapDefault'@
-- [/fmap/]
--   @'fmap' ≡ 'fmapDefault'@
--
-- Where an /applicative transformation/ is a function
--
-- @t :: (Applicative f, Applicative g) => f a -> g a@
--
-- preserving the 'Applicative' operations, i.e.
--
-- * Identity: @t ('pure' x) ≡ 'pure' x@
-- * Distributivity: @t (x '<*>' y) ≡ t x '<*>' t y@
traversableLaws ::
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Traversable f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Traversable f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Laws
traversableLaws = traversableLawsInternal

traversableLawsInternal :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Traversable f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Traversable f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Laws
traversableLawsInternal _ = Laws "Traversable"
  [
   (,) "Identity" $ property $ \(Apply (t :: f Integer)) ->
      nestedEq1 (traverse Identity t) (Identity t)
  , (,) "Composition" $ property $ \(Apply (t :: f Integer)) ->
      nestedEq1 (traverse (Compose . fmap func5 . func6) t) (Compose (fmap (traverse func5) (traverse func6 t)))
  , (,) "Sequence Identity" $ property $ \(Apply (t :: f Integer)) ->
      nestedEq1 (sequenceA (fmap Identity t)) (Identity t)
  , (,) "Sequence Composition" $ property $ \(Apply (t :: f (Triple (Triple Integer)))) ->
      nestedEq1 (sequenceA (fmap Compose t)) (Compose (fmap sequenceA (sequenceA t)))
  , (,) "foldMap" $ property $ \(Apply (t :: f Integer)) ->
      foldMap func3 t == foldMapDefault func3 t
  , (,) "fmap" $ property $ \(Apply (t :: f Integer)) ->
      eq1 (fmap func3 t) (fmapDefault func3 t)
  ]


#endif
