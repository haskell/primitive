{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Eq
  ( eqLaws
  ) where

import Data.Proxy (Proxy)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common (Laws(..))

-- | Tests the following properties:
--
-- [/Transitive/]
--   @a == b ∧ b == c ⇒ a == c@
-- [/Symmetric/]
--   @a == b ⇒ b == a@
-- [/Reflexive/]
--   @a == a@
--
-- Some of these properties involve implication. In the case that
-- the left hand side of the implication arrow does not hold, we
-- do not retry. Consequently, these properties only end up being
-- useful when the data type has a small number of inhabitants.
eqLaws :: (Eq a, Arbitrary a, Show a) => Proxy a -> Laws
eqLaws p = Laws "Eq"
  [ ("Transitive", eqTransitive p)
  , ("Symmetric", eqSymmetric p)
  , ("Reflexive", eqReflexive p)
  ]

eqTransitive :: forall a. (Show a, Eq a, Arbitrary a) => Proxy a -> Property
eqTransitive _ = property $ \(a :: a) b c -> case a == b of
  True -> case b == c of
    True -> a == c
    False -> a /= c
  False -> case b == c of
    True -> a /= c
    False -> True

eqSymmetric :: forall a. (Show a, Eq a, Arbitrary a) => Proxy a -> Property
eqSymmetric _ = property $ \(a :: a) b -> case a == b of
  True -> b == a
  False -> b /= a

eqReflexive :: forall a. (Show a, Eq a, Arbitrary a) => Proxy a -> Property
eqReflexive _ = property $ \(a :: a) -> a == a
