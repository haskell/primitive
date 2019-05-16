{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Semigroup
  ( -- * Laws
    semigroupLaws
  , commutativeSemigroupLaws
  , exponentialSemigroupLaws
  , idempotentSemigroupLaws
  , rectangularBandSemigroupLaws
  ) where

import Prelude hiding (foldr1)
import Data.Semigroup (Semigroup(..))
import Data.Proxy (Proxy)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common (Laws(..), SmallList(..), myForAllShrink)

import Data.Foldable (foldr1,toList)
import Data.List.NonEmpty (NonEmpty((:|)))

import qualified Data.List as L

-- | Tests the following properties:
--
-- [/Associative/]
--   @a '<>' (b '<>' c) ≡ (a '<>' b) '<>' c@
-- [/Concatenation/]
--   @'sconcat' as ≡ 'foldr1' ('<>') as@
-- [/Times/]
--   @'stimes' n a ≡ 'foldr1' ('<>') ('replicate' n a)@
semigroupLaws :: (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
semigroupLaws p = Laws "Semigroup"
  [ ("Associative", semigroupAssociative p)
  , ("Concatenation", semigroupConcatenation p)
  , ("Times", semigroupTimes p)
  ]

-- | Tests the following properties:
--
-- [/Commutative/]
--   @a '<>' b ≡ b '<>' a@
--
-- Note that this does not test associativity. Make sure to use
-- 'semigroupLaws' in addition to this set of laws.
commutativeSemigroupLaws :: (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
commutativeSemigroupLaws p = Laws "Commutative Semigroup"
  [ ("Commutative", semigroupCommutative p)
  ]

-- | Tests the following properties:
--
-- [/Idempotent/]
--   @a '<>' a ≡ a@
--
-- Note that this does not test associativity. Make sure to use
-- 'semigroupLaws' in addition to this set of laws. In literature,
-- this class of semigroup is known as a band.
idempotentSemigroupLaws :: (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
idempotentSemigroupLaws p = Laws "Idempotent Semigroup"
  [ ("Idempotent", semigroupIdempotent p)
  ]

-- | Tests the following properties:
--
-- [/Rectangular Band/]
--   @a '<>' b '<>' a ≡ a@
--
-- Note that this does not test associativity. Make sure to use
-- 'semigroupLaws' in addition to this set of laws.
rectangularBandSemigroupLaws :: (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
rectangularBandSemigroupLaws p = Laws "Rectangular Band Semigroup"
  [ ("Rectangular Band", semigroupRectangularBand p)
  ]

-- | Tests the following properties:
--
-- [/Exponential/]
--   @'stimes' n (a '<>' b) ≡ 'stimes' n a '<>' 'stimes' n b@
--
-- Note that this does not test associativity. Make sure to use
-- 'semigroupLaws' in addition to this set of laws.
exponentialSemigroupLaws :: (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
exponentialSemigroupLaws p = Laws "Exponential Semigroup"
  [ ("Exponential", semigroupExponential p)
  ]

semigroupAssociative :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semigroupAssociative _ = myForAllShrink True (const True)
  (\(a :: a,b,c) -> ["a = " ++ show a, "b = " ++ show b, "c = " ++ show c])
  "a <> (b <> c)"
  (\(a,b,c) -> a <> (b <> c))
  "(a <> b) <> c"
  (\(a,b,c) -> (a <> b) <> c)

semigroupCommutative :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semigroupCommutative _ = myForAllShrink True (const True)
  (\(a :: a,b) -> ["a = " ++ show a, "b = " ++ show b])
  "a <> b"
  (\(a,b) -> a <> b)
  "b <> a"
  (\(a,b) -> b <> a)

semigroupConcatenation :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semigroupConcatenation _ = myForAllShrink True (const True)
  (\(a, SmallList (as :: [a])) -> ["as = " ++ show (a :| as)])
  "sconcat as"
  (\(a, SmallList as) -> sconcat (a :| as))
  "foldr1 (<>) as"
  (\(a, SmallList as) -> foldr1 (<>) (a :| as))

semigroupTimes :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semigroupTimes _ = myForAllShrink True (\(_,n) -> n > 0)
  (\(a :: a, n :: Int) -> ["a = " ++ show a, "n = " ++ show n])
  "stimes n a"
  (\(a,n) -> stimes n a)
  "foldr1 (<>) (replicate n a)"
  (\(a,n) -> foldr1 (<>) (replicate n a))

semigroupExponential :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semigroupExponential _ = myForAllShrink True (\(_,_,n) -> n > 0)
  (\(a :: a, b, n :: Int) -> ["a = " ++ show a, "b = " ++ show b, "n = " ++ show n])
  "stimes n (a <> b)"
  (\(a,b,n) -> stimes n (a <> b))
  "stimes n a <> stimes n b"
  (\(a,b,n) -> stimes n a <> stimes n b)

semigroupIdempotent :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semigroupIdempotent _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "a <> a"
  (\a -> a <> a)
  "a"
  (\a -> a)

semigroupRectangularBand :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semigroupRectangularBand _ = myForAllShrink False (const True)
  (\(a :: a, b) -> ["a = " ++ show a, "b = " ++ show b])
  "a <> b <> a"
  (\(a,b) -> a <> b <> a)
  "a"
  (\(a,_) -> a)
