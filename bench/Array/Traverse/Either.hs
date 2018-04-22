{-# LANGUAGE BangPatterns #-}

module Array.Traverse.Either
  ( traverseEither
  ) where

import Control.Monad.ST
import Control.Monad.Trans.State.Strict
import Control.Monad.Primitive
import Data.Primitive.Array

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

