{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Storable
  ( storableLaws
  ) where

import Control.Applicative
import Data.Proxy (Proxy)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable

import GHC.Ptr (Ptr(..), plusPtr)
import System.IO.Unsafe
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property)

import qualified Data.List as L

import Test.QuickCheck.Classes.Common (Laws(..))

-- | Tests the following alternative properties:
--
-- [/Set-Get/]
--   @('pokeElemOff' ptr ix a >> 'peekElemOff' ptr ix') ≡ 'pure' a@
-- [/Get-Set/]
--   @('peekElemOff' ptr ix >> 'pokeElemOff' ptr ix a) ≡ 'pure' a@
storableLaws :: (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
storableLaws p = Laws "Storable"
  [ ("Set-Get (you get back what you put in)", storableSetGet p)
  , ("Get-Set (putting back what you got out has no effect)", storableGetSet p)
  , ("List Conversion Roundtrips", storableList p)
  , ("peekElemOff a i ≡ peek (plusPtr a (i * sizeOf undefined))", storablePeekElem p)
  , ("peekElemOff a i x ≡ poke (plusPtr a (i * sizeOf undefined)) x ≡ id ", storablePokeElem p)
  , ("peekByteOff a i ≡ peek (plusPtr a i)", storablePeekByte p)
  , ("peekByteOff a i x ≡ poke (plusPtr a i) x ≡ id ", storablePokeByte p)
  ]

arrayArbitrary :: forall a. (Arbitrary a, Storable a) => Int -> IO (Ptr a)
arrayArbitrary len = do
  let go ix xs = if ix == len
        then pure xs
        else do
          x <- generate (arbitrary :: Gen a)
          go (ix + 1) (x : xs)
  as <- go 0 []
  newArray as

storablePeekElem :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storablePeekElem _ = property $ \(as :: [a]) -> (not (L.null as)) ==> do
  let len = L.length as
  ix <- choose (0, len - 1)
  return $ unsafePerformIO $ do
    addr :: Ptr a <- arrayArbitrary len
    x <- peekElemOff addr ix
    y <- peek (addr `plusPtr` (ix * sizeOf (undefined :: a)))
    free addr
    return (x == y)

storablePokeElem :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storablePokeElem _ = property $ \(as :: [a]) (x :: a) -> (not (L.null as)) ==> do
  let len = L.length as
  ix <- choose (0, len - 1)
  return $ unsafePerformIO $ do
    addr :: Ptr a <- arrayArbitrary len
    pokeElemOff addr ix x
    u <- peekElemOff addr ix
    poke (addr `plusPtr` (ix * sizeOf x)) x
    v <- peekElemOff addr ix
    free addr
    return (u == v)

storablePeekByte :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storablePeekByte _ = property $ \(as :: [a]) -> (not (L.null as)) ==> do
  let len = L.length as
  off <- choose (0, len - 1)
  return $ unsafePerformIO $ do
    addr :: Ptr a <- arrayArbitrary len
    x :: a <- peekByteOff addr off
    y :: a <- peek (addr `plusPtr` off)
    free addr
    return (x == y)

storablePokeByte :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storablePokeByte _ = property $ \(as :: [a]) (x :: a) -> (not (L.null as)) ==> do
  let len = L.length as
  off <- choose (0, len - 1)
  return $ unsafePerformIO $ do
    addr :: Ptr a <- arrayArbitrary len
    pokeByteOff addr off x
    u :: a <- peekByteOff addr off
    poke (addr `plusPtr` off) x
    v :: a <- peekByteOff addr off
    free addr
    return (u == v)

storableSetGet :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storableSetGet _ = property $ \(a :: a) len -> (len > 0) ==> do
  ix <- choose (0,len - 1)
  return $ unsafePerformIO $ do
    ptr :: Ptr a <- arrayArbitrary len
    pokeElemOff ptr ix a
    a' <- peekElemOff ptr ix
    free ptr
    return (a == a')

storableGetSet :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storableGetSet _ = property $ \(as :: [a]) -> (not (L.null as)) ==> do
  let len = L.length as
  ix <- choose (0,len - 1)
  return $ unsafePerformIO $ do
    ptrA <- newArray as
    ptrB <- arrayArbitrary len
    copyArray ptrB ptrA len
    a <- peekElemOff ptrA ix
    pokeElemOff ptrA ix a
    res <- arrayEq ptrA ptrB len
    free ptrA
    free ptrB
    return res

storableList :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storableList _ = property $ \(as :: [a]) -> unsafePerformIO $ do
  let len = L.length as
  ptr <- newArray as
  let rebuild :: Int -> IO [a]
      rebuild !ix = if ix < len
        then (:) <$> peekElemOff ptr ix <*> rebuild (ix + 1)
        else return []
  asNew <- rebuild 0
  free ptr
  return (as == asNew)

arrayEq :: forall a. (Storable a, Eq a) => Ptr a -> Ptr a -> Int -> IO Bool
arrayEq ptrA ptrB len = go 0 where
  go !i = if i < len
    then do
      a <- peekElemOff ptrA i
      b <- peekElemOff ptrB i
      if a == b
        then go (i + 1)
        else return False
    else return True
