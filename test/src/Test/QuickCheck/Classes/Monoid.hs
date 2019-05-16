{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Monoid
  ( monoidLaws
  , commutativeMonoidLaws
  ) where

import Data.Monoid
import Data.Proxy (Proxy)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common (Laws(..), SmallList(..), myForAllShrink)

-- | Tests the following properties:
--
-- [/Associative/]
--   @mappend a (mappend b c) ≡ mappend (mappend a b) c@
-- [/Left Identity/]
--   @mappend mempty a ≡ a@
-- [/Right Identity/]
--   @mappend a mempty ≡ a@
-- [/Concatenation/]
--   @mconcat as ≡ foldr mappend mempty as@
monoidLaws :: (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
monoidLaws p = Laws "Monoid"
  [ ("Associative", monoidAssociative p)
  , ("Left Identity", monoidLeftIdentity p)
  , ("Right Identity", monoidRightIdentity p)
  , ("Concatenation", monoidConcatenation p)
  ]

-- | Tests the following properties:
--
-- [/Commutative/]
--   @mappend a b ≡ mappend b a@
--
-- Note that this does not test associativity or identity. Make sure to use
-- 'monoidLaws' in addition to this set of laws.
commutativeMonoidLaws :: (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
commutativeMonoidLaws p = Laws "Commutative Monoid"
  [ ("Commutative", monoidCommutative p)
  ]

monoidConcatenation :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidConcatenation _ = myForAllShrink True (const True)
  (\(SmallList (as :: [a])) -> ["as = " ++ show as])
  "mconcat as"
  (\(SmallList as) -> mconcat as)
  "foldr mappend mempty as"
  (\(SmallList as) -> foldr mappend mempty as)

monoidAssociative :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidAssociative _ = myForAllShrink True (const True)
  (\(a :: a,b,c) -> ["a = " ++ show a, "b = " ++ show b, "c = " ++ show c])
  "mappend a (mappend b c)"
  (\(a,b,c) -> mappend a (mappend b c))
  "mappend (mappend a b) c"
  (\(a,b,c) -> mappend (mappend a b) c)

monoidLeftIdentity :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidLeftIdentity _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "mappend mempty a"
  (\a -> mappend mempty a)
  "a"
  (\a -> a)

monoidRightIdentity :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidRightIdentity _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "mappend a mempty"
  (\a -> mappend a mempty)
  "a"
  (\a -> a)

monoidCommutative :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidCommutative _ = myForAllShrink True (const True)
  (\(a :: a,b) -> ["a = " ++ show a, "b = " ++ show b])
  "mappend a b"
  (\(a,b) -> mappend a b)
  "mappend b a"
  (\(a,b) -> mappend b a)
