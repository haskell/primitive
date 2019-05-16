{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

{-| Module      : Test.QuickCheck.Classes.ShowRead
    Description : Properties for testing the interaction between the Show and Read
                  type classes.
-}
module Test.QuickCheck.Classes.ShowRead
  ( showReadLaws
  ) where

import Data.Proxy (Proxy)
import Test.QuickCheck
import Text.Read (readListDefault)
import Text.Show (showListWith)

import Test.QuickCheck.Classes.Common (Laws(..), ShowReadPrecedence(..),
  SmallList(..), myForAllShrink)
import Test.QuickCheck.Classes.Compat (readMaybe)

-- | Tests the following properties:
--
-- [/Partial Isomorphism: 'show' \/ 'read'/]
--   @'readMaybe' ('show' a) ≡ 'Just' a@
-- [/Partial Isomorphism: 'show' \/ 'read' with initial space/]
--   @'readMaybe' (" " ++ 'show' a) ≡ 'Just' a@
-- [/Partial Isomorphism: 'showsPrec' \/ 'readsPrec'/]
--   @(a,"") \`elem\` 'readsPrec' p ('showsPrec' p a "")@
-- [/Partial Isomorphism: 'showList' \/ 'readList'/]
--   @(as,"") \`elem\` 'readList' ('showList' as "")@
-- [/Partial Isomorphism: 'showListWith' 'shows' \/ 'readListDefault'/]
--   @(as,"") \`elem\` 'readListDefault' ('showListWith' 'shows' as "")@
--
-- /Note:/ When using @base-4.5@ or older, a shim implementation
-- of 'readMaybe' is used.
--
showReadLaws :: (Show a, Read a, Eq a, Arbitrary a) => Proxy a -> Laws
showReadLaws p = Laws "Show/Read"
  [ ("Partial Isomorphism: show/read", showReadPartialIsomorphism p)
  , ("Partial Isomorphism: show/read with initial space", showReadSpacePartialIsomorphism p)
  , ("Partial Isomorphism: showsPrec/readsPrec", showsPrecReadsPrecPartialIsomorphism p)
  , ("Partial Isomorphism: showList/readList", showListReadListPartialIsomorphism p)
  , ("Partial Isomorphism: showListWith shows / readListDefault",
     showListWithShowsReadListDefaultPartialIsomorphism p)
  ]


showReadPartialIsomorphism :: forall a.
  (Show a, Read a, Arbitrary a, Eq a) => Proxy a -> Property
showReadPartialIsomorphism _ =
  myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  ("readMaybe (show a)")
  (\a -> readMaybe (show a))
  ("Just a")
  (\a -> Just a)

showReadSpacePartialIsomorphism :: forall a.
  (Show a, Read a, Arbitrary a, Eq a) => Proxy a -> Property
showReadSpacePartialIsomorphism _ =
  myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  ("readMaybe (\" \" ++ show a)")
  (\a -> readMaybe (" " ++ show a))
  ("Just a")
  (\a -> Just a)

showsPrecReadsPrecPartialIsomorphism :: forall a.
  (Show a, Read a, Arbitrary a, Eq a) => Proxy a -> Property
showsPrecReadsPrecPartialIsomorphism _ =
  property $ \(a :: a) (ShowReadPrecedence p) ->
    (a,"") `elem` readsPrec p (showsPrec p a "")

showListReadListPartialIsomorphism :: forall a.
  (Show a, Read a, Arbitrary a, Eq a) => Proxy a -> Property
showListReadListPartialIsomorphism _ =
  property $ \(SmallList (as :: [a])) ->
    (as,"") `elem` readList (showList as "")

showListWithShowsReadListDefaultPartialIsomorphism :: forall a.
  (Show a, Read a, Arbitrary a, Eq a) => Proxy a -> Property
showListWithShowsReadListDefaultPartialIsomorphism _ =
  property $ \(SmallList (as :: [a])) ->
    (as,"") `elem` readListDefault (showListWith shows as "")

