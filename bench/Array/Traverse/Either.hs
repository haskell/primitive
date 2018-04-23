{-# LANGUAGE BangPatterns #-}

module Array.Traverse.Either
  ( traverseEither
  ) where

import Control.Monad.ST
import Control.Monad.Trans.State.Strict
import Control.Monad.Primitive
import Data.Primitive.Array

-- This is a specialization of traverse, where the applicative is
-- chosen to be Either. In the benchmark suite, this implementation
-- is compared against an implementation that uses ExceptT to see
-- if GHC is able to optimize the ExceptT variant to code as efficient
-- as this. At the time this test was written (2018-04-23), GHC does
-- appear to optimize the ExceptT variant so that it performs as well
-- as this one.
{-# INLINE traverseEither #-}
traverseEither ::
     (a -> Either e b)
  -> Array a
  -> Either e (Array b)
traverseEither f = \ !ary ->
  let
    !sz = sizeofArray ary
    go !i !mary
      | i == sz = do
          r <- unsafeFreezeArray mary
          return (Right r)
      | otherwise = do
          a <- indexArrayM ary i
          case f a of
            Left e -> return (Left e)
            Right b -> do
              writeArray mary i b
              go (i + 1) mary
  in runST $ do
    mary <- newArray sz badTraverseValue
    go 0 mary

badTraverseValue :: a
badTraverseValue = die "traverseEither" "bad indexing"
{-# NOINLINE badTraverseValue #-}

die :: String -> String -> a
die fun problem = error $ "Array.Traverse.Either" ++ fun ++ ": " ++ problem

