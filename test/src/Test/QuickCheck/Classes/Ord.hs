{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Ord
  ( ordLaws
  ) where

import Data.Proxy (Proxy)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common (Laws(..))

-- | Tests the following properties:
--
-- [/Antisymmetry/]
--   @a ≤ b ∧ b ≤ a ⇒ a = b@ 
-- [/Transitivity/]
--   @a ≤ b ∧ b ≤ c ⇒ a ≤ c@
-- [/Totality/]
--   @a ≤ b ∨ a > b@
ordLaws :: (Ord a, Arbitrary a, Show a) => Proxy a -> Laws
ordLaws p = Laws "Ord"
  [ ("Antisymmetry", ordAntisymmetric p)
  , ("Transitivity", ordTransitive p)
  , ("Totality", ordTotal p)
  ]

ordAntisymmetric :: forall a. (Show a, Ord a, Arbitrary a) => Proxy a -> Property
ordAntisymmetric _ = property $ \(a :: a) b -> ((a <= b) && (b <= a)) == (a == b)

ordTotal :: forall a. (Show a, Ord a, Arbitrary a) => Proxy a -> Property
ordTotal _ = property $ \(a :: a) b -> ((a <= b) || (b <= a)) == True

-- Technically, this tests something a little stronger than it is supposed to.
-- But that should be alright since this additional strength is implied by
-- the rest of the Ord laws.
ordTransitive :: forall a. (Show a, Ord a, Arbitrary a) => Proxy a -> Property
ordTransitive _ = property $ \(a :: a) b c -> case (compare a b, compare b c) of
  (LT,LT) -> a < c
  (LT,EQ) -> a < c
  (LT,GT) -> True
  (EQ,LT) -> a < c
  (EQ,EQ) -> a == c
  (EQ,GT) -> a > c
  (GT,LT) -> True
  (GT,EQ) -> a > c
  (GT,GT) -> a > c
