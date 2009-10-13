{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Data.Primitive.Addr (
  Addr(..),

  nullAddr, plusAddr, minusAddr, remAddr,
  indexOffAddr, readOffAddr, writeOffAddr
) where

import Control.Monad.Primitive
import Data.Primitive.Types

import GHC.Base ( Int(..) )
import GHC.Prim

instance Eq Addr where
  Addr a# == Addr b# = eqAddr# a# b#
  Addr a# /= Addr b# = neAddr# a# b#

instance Ord Addr where
  Addr a# > Addr b# = gtAddr# a# b#
  Addr a# >= Addr b# = geAddr# a# b#
  Addr a# < Addr b# = ltAddr# a# b#
  Addr a# <= Addr b# = leAddr# a# b#

nullAddr :: Addr
nullAddr = Addr nullAddr#

infixl 6 `plusAddr`, `minusAddr`
infixl 7 `remAddr`

plusAddr :: Addr -> Int -> Addr
plusAddr (Addr a#) (I# i#) = Addr (plusAddr# a# i#)

minusAddr :: Addr -> Addr -> Int
minusAddr (Addr a#) (Addr b#) = I# (minusAddr# a# b#)

remAddr :: Addr -> Int -> Int
remAddr (Addr a#) (I# i#) = I# (remAddr# a# i#)

indexOffAddr :: Prim a => Addr -> Int -> a
{-# INLINE indexOffAddr #-}
indexOffAddr (Addr addr#) (I# i#) = indexOffAddr# addr# i#

readOffAddr :: (Prim a, PrimMonad m) => Addr -> Int -> m a
{-# INLINE readOffAddr #-}
readOffAddr (Addr addr#) (I# i#) = primitive (readOffAddr# addr# i#)

writeOffAddr :: (Prim a, PrimMonad m) => Addr -> Int -> a -> m ()
{-# INLINE writeOffAddr #-}
writeOffAddr (Addr addr#) (I# i#) x = primitive_ (writeOffAddr# addr# i# x)

