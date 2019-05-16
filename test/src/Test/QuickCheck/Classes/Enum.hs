{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Enum
  ( enumLaws
  , boundedEnumLaws
  ) where

import Data.Proxy (Proxy)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common (Laws(..), myForAllShrink)

-- | Tests the following properties:
--
-- [/Succ Pred Identity/]
--   @'succ' ('pred' x) ≡ x@
-- [/Pred Succ Identity/]
--   @'pred' ('succ' x) ≡ x@
--
-- This only works for @Enum@ types that are not bounded, meaning
-- that 'succ' and 'pred' must be total. This means that these property
-- tests work correctly for types like 'Integer' but not for 'Int'.
--
-- Sadly, there is not a good way to test 'fromEnum' and 'toEnum',
-- since many types that have reasonable implementations for 'succ'
-- and 'pred' have more inhabitants than 'Int' does.
enumLaws :: (Enum a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
enumLaws p = Laws "Enum"
  [ ("Succ Pred Identity", succPredIdentity p)
  , ("Pred Succ Identity", predSuccIdentity p)
  ]

-- | Tests the same properties as 'enumLaws' except that it requires
-- the type to have a 'Bounded' instance. These tests avoid taking the
-- successor of the maximum element or the predecessor of the minimal
-- element.
boundedEnumLaws :: (Enum a, Bounded a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
boundedEnumLaws p = Laws "Enum"
  [ ("Succ Pred Identity", succPredBoundedIdentity p)
  , ("Pred Succ Identity", predSuccBoundedIdentity p)
  ]

succPredIdentity :: forall a. (Enum a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
succPredIdentity _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "succ (pred x)"
  (\a -> succ (pred a))
  "x"
  (\a -> a)

predSuccIdentity :: forall a. (Enum a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
predSuccIdentity _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "pred (succ x)"
  (\a -> pred (succ a))
  "x"
  (\a -> a)

succPredBoundedIdentity :: forall a. (Enum a, Bounded a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
succPredBoundedIdentity _ = myForAllShrink False (\a -> a /= minBound)
  (\(a :: a) -> ["a = " ++ show a])
  "succ (pred x)"
  (\a -> succ (pred a))
  "x"
  (\a -> a)

predSuccBoundedIdentity :: forall a. (Enum a, Bounded a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
predSuccBoundedIdentity _ = myForAllShrink False (\a -> a /= maxBound)
  (\(a :: a) -> ["a = " ++ show a])
  "pred (succ x)"
  (\a -> pred (succ a))
  "x"
  (\a -> a)

