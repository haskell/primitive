{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import Gauge
import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Monoid
import Data.Primitive
import Data.Primitive.Array
import Data.Primitive.ByteArray
import Data.Primitive.Types
import Data.Primitive.SmallArray
import Data.Word
import Data.Proxy (Proxy(..))
import Control.DeepSeq
import Control.Monad.Trans.State.Strict

import qualified Array.Traverse.Unsafe
import qualified Array.Traverse.Closure

main :: IO ()
main = defaultMain
  [ bgroup "Array"
    [ bgroup "traverse"
      [ bench "closure" (nf (\x -> runST (runStateT (Array.Traverse.Closure.traversePoly cheap x) 0)) numbers)
      , bench "unsafe" (nf (\x -> runST (runStateT (Array.Traverse.Unsafe.traversePoly cheap x) 0)) numbers)
      ]
    ]
  ]

cheap :: Int -> StateT Int (ST s) Int
cheap i = modify (\x -> x + i) >> return (i * i)

numbers :: Array Int
numbers = fromList (enumFromTo 0 10000)

instance NFData a => NFData (Array a) where
  rnf ary = go 0 where
    !sz = sizeofArray ary
    go !ix = if ix < sz
      then ()
      else rnf (indexArray ary ix) `seq` go (ix + 1)
