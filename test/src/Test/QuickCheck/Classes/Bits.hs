{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Bits
  (
#if MIN_VERSION_base(4,7,0)
  bitsLaws
#endif
  ) where

import Data.Bits
import Data.Proxy (Proxy)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property)

--import qualified Data.Set as S

import Test.QuickCheck.Classes.Common (Laws(..), myForAllShrink)

-- | Tests the following properties:
--
-- [/Conjunction Idempotence/]
--   @n .&. n ≡ n@
-- [/Disjunction Idempotence/]
--   @n .|. n ≡ n@
-- [/Double Complement/]
--   @complement (complement n) ≡ n@
-- [/Set Bit/]
--   @setBit n i ≡ n .|. bit i@
-- [/Clear Bit/]
--   @clearBit n i ≡ n .&. complement (bit i)@
-- [/Complement Bit/]
--   @complementBit n i ≡ xor n (bit i)@
-- [/Clear Zero/]
--   @clearBit zeroBits i ≡ zeroBits@
-- [/Set Zero/]
--   @setBit zeroBits i ≡ bit i@
-- [/Test Zero/]
--   @testBit zeroBits i ≡ False@
-- [/Pop Zero/]
--   @popCount zeroBits ≡ 0@
-- [/Count Leading Zeros of Zero/]
--   @countLeadingZeros zeroBits ≡ finiteBitSize ⊥@
-- [/Count Trailing Zeros of Zero/]
--   @countTrailingZeros zeroBits ≡ finiteBitSize ⊥@
--
-- All of the useful instances of the 'Bits' typeclass
-- also have 'FiniteBits' instances, so these property
-- tests actually require that instance as well.
--
-- /Note:/ This property test is only available when
-- using @base-4.7@ or newer.
#if MIN_VERSION_base(4,7,0)
bitsLaws :: (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Laws
bitsLaws p = Laws "Bits"
  [ ("Conjunction Idempotence", bitsConjunctionIdempotence p)
  , ("Disjunction Idempotence", bitsDisjunctionIdempotence p)
  , ("Double Complement", bitsDoubleComplement p)
  , ("Set Bit", bitsSetBit p)
  , ("Clear Bit", bitsClearBit p)
  , ("Complement Bit", bitsComplementBit p)
  , ("Clear Zero", bitsClearZero p)
  , ("Set Zero", bitsSetZero p)
  , ("Test Zero", bitsTestZero p)
  , ("Pop Zero", bitsPopZero p)
#if MIN_VERSION_base(4,8,0)
  , ("Count Leading Zeros of Zero", bitsCountLeadingZeros p)
  , ("Count Trailing Zeros of Zero", bitsCountTrailingZeros p)
#endif
  ]
#endif

#if MIN_VERSION_base(4,7,0)
newtype BitIndex a = BitIndex Int

instance FiniteBits a => Arbitrary (BitIndex a) where
  arbitrary = let n = finiteBitSize (undefined :: a) in if n > 0
    then fmap BitIndex (choose (0,n - 1))
    else return (BitIndex 0)
  shrink (BitIndex x) = if x > 0 then map BitIndex (S.toList (S.fromList [x - 1, div x 2, 0])) else []

bitsConjunctionIdempotence :: forall a. (Bits a, Arbitrary a, Show a) => Proxy a -> Property
bitsConjunctionIdempotence _ = myForAllShrink False (const True)
  (\(n :: a) -> ["n = " ++ show n])
  "n .&. n"
  (\n -> n .&. n)
  "n"
  (\n -> n)

bitsDisjunctionIdempotence :: forall a. (Bits a, Arbitrary a, Show a) => Proxy a -> Property
bitsDisjunctionIdempotence _ = myForAllShrink False (const True)
  (\(n :: a) -> ["n = " ++ show n])
  "n .|. n"
  (\n -> n .|. n)
  "n"
  (\n -> n)

bitsDoubleComplement :: forall a. (Bits a, Arbitrary a, Show a) => Proxy a -> Property
bitsDoubleComplement _ = myForAllShrink False (const True)
  (\(n :: a) -> ["n = " ++ show n])
  "complement (complement n)"
  (\n -> complement (complement n))
  "n"
  (\n -> n)

bitsSetBit :: forall a. (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Property
bitsSetBit _ = myForAllShrink True (const True)
  (\(n :: a, BitIndex i :: BitIndex a) -> ["n = " ++ show n, "i = " ++ show i])
  "setBit n i"
  (\(n,BitIndex i) -> setBit n i)
  "n .|. bit i"
  (\(n,BitIndex i) -> n .|. bit i)

bitsClearBit :: forall a. (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Property
bitsClearBit _ = myForAllShrink True (const True)
  (\(n :: a, BitIndex i :: BitIndex a) -> ["n = " ++ show n, "i = " ++ show i])
  "clearBit n i"
  (\(n,BitIndex i) -> clearBit n i)
  "n .&. complement (bit i)"
  (\(n,BitIndex i) -> n .&. complement (bit i))

bitsComplementBit :: forall a. (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Property
bitsComplementBit _ = myForAllShrink True (const True)
  (\(n :: a, BitIndex i :: BitIndex a) -> ["n = " ++ show n, "i = " ++ show i])
  "complementBit n i"
  (\(n,BitIndex i) -> complementBit n i)
  "xor n (bit i)"
  (\(n,BitIndex i) -> xor n (bit i))

bitsClearZero :: forall a. (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Property
bitsClearZero _ = myForAllShrink False (const True)
  (\(BitIndex n :: BitIndex a) -> ["n = " ++ show n])
  "clearBit zeroBits n"
  (\(BitIndex n) -> clearBit zeroBits n :: a)
  "zeroBits"
  (\_ -> zeroBits)

bitsSetZero :: forall a. (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Property
bitsSetZero _ = myForAllShrink True (const True)
  (\(BitIndex i :: BitIndex a) -> ["i = " ++ show i])
  "setBit zeroBits i"
  (\(BitIndex i) -> setBit (zeroBits :: a) i)
  "bit i"
  (\(BitIndex i) -> bit i)

bitsTestZero :: forall a. (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Property
bitsTestZero _ = myForAllShrink True (const True)
  (\(BitIndex i :: BitIndex a) -> ["i = " ++ show i])
  "testBit zeroBits i"
  (\(BitIndex i) -> testBit (zeroBits :: a) i)
  "False"
  (\_ -> False)

bitsPopZero :: forall a. (Bits a, Arbitrary a, Show a) => Proxy a -> Property
bitsPopZero _ = myForAllShrink True (const True)
  (\() -> [])
  "popCount zeroBits"
  (\() -> popCount (zeroBits :: a))
  "0"
  (\() -> 0)
#endif

#if MIN_VERSION_base(4,8,0)
bitsCountLeadingZeros :: forall a. (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Property
bitsCountLeadingZeros _ = myForAllShrink True (const True)
  (\() -> [])
  "countLeadingZeros zeroBits"
  (\() -> countLeadingZeros (zeroBits :: a))
  "finiteBitSize undefined"
  (\() -> finiteBitSize (undefined :: a))

bitsCountTrailingZeros :: forall a. (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Property
bitsCountTrailingZeros _ = myForAllShrink True (const True)
  (\() -> [])
  "countTrailingZeros zeroBits"
  (\() -> countTrailingZeros (zeroBits :: a))
  "finiteBitSize undefined"
  (\() -> finiteBitSize (undefined :: a))
#endif
