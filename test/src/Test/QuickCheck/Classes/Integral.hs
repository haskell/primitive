{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Integral
  ( integralLaws
  ) where

import Data.Proxy (Proxy)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common (Laws(..), myForAllShrink)

-- | Tests the following properties:
--
-- [/Quotient Remainder/]
--   @(quot x y) * y + (rem x y) ≡ x@
-- [/Division Modulus/]
--   @(div x y) * y + (mod x y) ≡ x@
-- [/Integer Roundtrip/]
--   @fromInteger (toInteger x) ≡ x@
integralLaws :: (Integral a, Arbitrary a, Show a) => Proxy a -> Laws
integralLaws p = Laws "Integral"
  [ ("Quotient Remainder", integralQuotientRemainder p)
  , ("Division Modulus", integralDivisionModulus p)
  , ("Integer Roundtrip", integralIntegerRoundtrip p)
  ]

integralQuotientRemainder :: forall a. (Integral a, Arbitrary a, Show a) => Proxy a -> Property
integralQuotientRemainder _ = myForAllShrink False (\(_,y) -> y /= 0)
  (\(x :: a, y) -> ["x = " ++ show x, "y = " ++ show y])
  "(quot x y) * y + (rem x y)"
  (\(x,y) -> (quot x y) * y + (rem x y))
  "x"
  (\(x,_) -> x)

integralDivisionModulus :: forall a. (Integral a, Arbitrary a, Show a) => Proxy a -> Property
integralDivisionModulus _ = myForAllShrink False (\(_,y) -> y /= 0)
  (\(x :: a, y) -> ["x = " ++ show x, "y = " ++ show y])
  "(div x y) * y + (mod x y)"
  (\(x,y) -> (div x y) * y + (mod x y))
  "x"
  (\(x,_) -> x)

integralIntegerRoundtrip :: forall a. (Integral a, Arbitrary a, Show a) => Proxy a -> Property
integralIntegerRoundtrip _ = myForAllShrink False (const True)
  (\(x :: a) -> ["x = " ++ show x])
  "fromInteger (toInteger x)"
  (\x -> fromInteger (toInteger x))
  "x"
  (\x -> x)
