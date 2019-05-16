{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Bifoldable
  (
#if HAVE_BINARY_LAWS
    bifoldableLaws
  , bifoldableFunctorLaws
#endif
  ) where

#if HAVE_BINARY_LAWS
import Data.Bifoldable(Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Test.QuickCheck hiding ((.&.))
import Data.Functor.Classes (Eq2,Show2)
import Test.QuickCheck.Property (Property)
import Data.Monoid
import Data.Orphans ()
import Test.QuickCheck.Classes.Common
#endif

#if HAVE_BINARY_LAWS

-- | Tests the following 'Bifunctor' properties:
--
-- [/Bifold Identity/]
--   @'bifold' ≡ 'bifoldMap' 'id' 'id'@  
-- [/BifoldMap Identity/]
--   @'bifoldMap' f g ≡ 'bifoldr' ('mappend' '.' f) ('mappend' '.' g) 'mempty'@
-- [/Bifoldr Identity/] 
--   @'bifoldr' f g z t ≡ 'appEndo' ('bifoldMap' ('Endo' '.' f) ('Endo' '.' g) t) z@
--
-- /Note/: This property test is only available when this package is built with
-- @base-4.10+@ or @transformers-0.5+@.
bifoldableLaws :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Bifoldable f, forall a b. (Eq a, Eq b) => Eq (f a b), forall a b. (Show a, Show b) => Show (f a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b))
#else
  (Bifoldable f, Eq2 f, Show2 f, Arbitrary2 f)
#endif
  => proxy f -> Laws
bifoldableLaws p = Laws "Bifoldable"
  [ ("Bifold Identity", bifoldIdentity p)
  , ("BifoldMap Identity", bifoldMapIdentity p)
  , ("Bifoldr Identity", bifoldrIdentity p)
  ]

-- | Tests the following 'Bifunctor'/'Bifoldable' properties:
--
-- [/Bifold Identity/]
--   @'bifoldMap' f g ≡ 'bifold' '.' 'bimap' f g@
-- [/BifoldMap Identity/]
--   @'bifoldMap' f g '.' 'bimap' h i ≡ 'bifoldMap' (f '.' h) (g '.' i)@
--
-- /Note/: This property test is only available when this package is built with
-- @base-4.10+@ or @transformers-0.5+@.
bifoldableFunctorLaws :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Bifoldable f, Bifunctor f, forall a b. (Eq a, Eq b) => Eq (f a b), forall a b. (Show a, Show b) => Show (f a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b))
#else
  (Bifoldable f, Bifunctor f, Eq2 f, Show2 f, Arbitrary2 f)
#endif
  => proxy f -> Laws
bifoldableFunctorLaws p = Laws "Bifoldable/Bifunctor"
  [ ("Bifoldable Bifunctor Law", bifoldableFunctorLaw p)
  , ("Bifoldable Bifunctor Law Implication", bifoldableFunctorImplication p)
  ]

bifoldableFunctorLaw :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Bifoldable f, Bifunctor f, forall a b. (Eq a, Eq b) => Eq (f a b), forall a b. (Show a, Show b) => Show (f a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b))
#else
  (Bifoldable f, Bifunctor f, Eq2 f, Show2 f, Arbitrary2 f)
#endif
  => proxy f -> Property
bifoldableFunctorLaw _ = property $ \(Apply2 (x :: f Integer Integer)) -> bifoldMap Sum Sum x == (bifold (bimap Sum Sum x))

bifoldableFunctorImplication :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Bifoldable f, Bifunctor f, forall a b. (Eq a, Eq b) => Eq (f a b), forall a b. (Show a, Show b) => Show (f a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b))
#else
  (Bifoldable f, Bifunctor f, Eq2 f, Show2 f, Arbitrary2 f)
#endif
  => proxy f -> Property
bifoldableFunctorImplication _ = property $ \(Apply2 (x :: f Integer Integer)) -> bifoldMap Sum Sum (bimap Product Product x) == bifoldMap (Sum . Product) (Sum . Product) x

bifoldIdentity :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Bifoldable f, forall a b. (Eq a, Eq b) => Eq (f a b), forall a b. (Show a, Show b) => Show (f a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b))
#else
  (Bifoldable f, Eq2 f, Show2 f, Arbitrary2 f)
#endif
  => proxy f -> Property
bifoldIdentity _ = property $ \(Apply2 (x :: f (Sum Integer) (Sum Integer))) -> (bifold x) == (bifoldMap id id x)

bifoldMapIdentity :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Bifoldable f, forall a b. (Eq a, Eq b) => Eq (f a b), forall a b. (Show a, Show b) => Show (f a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b))
#else
  (Bifoldable f, Eq2 f, Show2 f, Arbitrary2 f)
#endif
  => proxy f -> Property
bifoldMapIdentity _ = property $ \(Apply2 (x :: f Integer Integer)) -> bifoldMap Sum Sum x == bifoldr (mappend . Sum) (mappend . Sum) mempty x

bifoldrIdentity :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Bifoldable f, forall a b. (Eq a, Eq b) => Eq (f a b), forall a b. (Show a, Show b) => Show (f a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b))
#else
  (Bifoldable f, Eq2 f, Show2 f, Arbitrary2 f)
#endif
  => proxy f -> Property
bifoldrIdentity _ = property $ \(Apply2 (x :: f Integer Integer)) ->
  let f _ _ = mempty
      g _ _ = mempty
  in bifoldr f g (mempty :: Sum Integer) x == appEndo (bifoldMap (Endo . f) (Endo . g) x) mempty

#endif