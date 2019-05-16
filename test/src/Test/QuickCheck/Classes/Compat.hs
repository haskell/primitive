{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

module Test.QuickCheck.Classes.Compat
  ( isTrue#
  , eq1

  , readMaybe
  ) where

#if MIN_VERSION_base(4,6,0)
import Text.Read (readMaybe)
#else
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadPrec (lift, minPrec, readPrec_to_S)
import Text.Read (readPrec)
#endif

#if MIN_VERSION_base(4,7,0)
import GHC.Exts (isTrue#)
#endif

import qualified Data.Functor.Classes as C


#if !MIN_VERSION_base(4,6,0)
readMaybe :: Read a => String -> Maybe a
readMaybe s =
  case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
    [x] -> Just x
    _   -> Nothing
 where
  read' =
    do x <- readPrec
       lift skipSpaces
       return x
#endif

#if !MIN_VERSION_base(4,7,0)
isTrue# :: Bool -> Bool
isTrue# b = b
#endif


#if HAVE_QUANTIFIED_CONSTRAINTS
eq1 :: (forall a. Eq a => Eq (f a), Eq a) => f a -> f a -> Bool
eq1 = (==)
#else
eq1 :: (C.Eq1 f, Eq a) => f a -> f a -> Bool
#if   !(MIN_VERSION_transformers(0,5,0))
 -- checking for transformers 0.4 by another name
eq1 = C.eq1
#else
eq1 = C.liftEq (==)
#endif
#endif




