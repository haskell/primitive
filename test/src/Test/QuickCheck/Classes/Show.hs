{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

{-| Module      : Test.QuickCheck.Classes.Show
    Description : Properties for testing the properties of the Show type class.
-}
module Test.QuickCheck.Classes.Show
  ( showLaws
  ) where

import Data.Proxy (Proxy)
import Test.QuickCheck (Arbitrary, Property, property)

import Test.QuickCheck.Classes.Common (Laws(..), ShowReadPrecedence(..))

-- | Tests the following properties:
--
-- [/Show/]
-- @'show' a ≡ 'showsPrec' 0 a ""@
-- [/Equivariance: 'showsPrec'/]
-- @'showsPrec' p a r '++' s ≡ 'showsPrec' p a (r '++' s)@
-- [/Equivariance: 'showList'/]
-- @'showList' as r '++' s ≡ 'showList' as (r '++' s)@
--
showLaws :: (Show a, Arbitrary a) => Proxy a -> Laws
showLaws p = Laws "Show"
  [ ("Show", showShowsPrecZero p)
  , ("Equivariance: showsPrec", equivarianceShowsPrec p)
  , ("Equivariance: showList", equivarianceShowList p)
  ]

showShowsPrecZero :: forall a. (Show a, Arbitrary a) => Proxy a -> Property
showShowsPrecZero _ =
  property $ \(a :: a) ->
    show a == showsPrec 0 a ""

equivarianceShowsPrec :: forall a.
  (Show a, Arbitrary a) => Proxy a -> Property
equivarianceShowsPrec _ =
  property $ \(ShowReadPrecedence p) (a :: a) (r :: String) (s :: String) ->
    showsPrec p a r ++ s == showsPrec p a (r ++ s)

equivarianceShowList :: forall a.
  (Show a, Arbitrary a) => Proxy a -> Property
equivarianceShowList _ =
  property $ \(as :: [a]) (r :: String) (s :: String) ->
    showList as r ++ s == showList as (r ++ s)
