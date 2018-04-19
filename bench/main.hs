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

-- These are fixed implementations of certain operations. In the event
-- that primitive changes its implementation of a function, these
-- implementations stay the same. They are helpful for ensuring that
-- something that is a performance win in one version of GHC doesn't
-- become a regression later. They are also helpful for evaluating
-- how well different implementation hold up in different scenarios.
import qualified Array.Traverse.Unsafe
import qualified Array.Traverse.Closure

-- These are particular scenarios that are tested against the
-- implementations actually used by primitive.
import qualified ByteArray.Compare
import qualified PrimArray.Traverse

main :: IO ()
main = defaultMain
  [ bgroup "Array"
    [ bgroup "implementations"
      [ bgroup "traverse"
        [ bench "closure" (nf (\x -> runST (runStateT (Array.Traverse.Closure.traversePoly cheap x) 0)) numbers)
        , bench "unsafe" (nf (\x -> runST (runStateT (Array.Traverse.Unsafe.traversePoly cheap x) 0)) numbers)
        ]
      ]
    ]
  , bgroup "ByteArray"
    [ bgroup "compare"
      [ bench "small" (whnf ByteArray.Compare.benchmark ByteArray.Compare.argumentSmall)
      , bench "medium" (whnf ByteArray.Compare.benchmark ByteArray.Compare.argumentMedium)
      , bench "large" (whnf ByteArray.Compare.benchmark ByteArray.Compare.argumentLarge)
      ]
    ]
  , bgroup "PrimArray"
    [ bgroup "traverse"
      [ bgroup "Maybe"
        [ bench "Applicative" (whnf PrimArray.Traverse.benchmarkApplicative PrimArray.Traverse.argument)
        , bench "PrimMonad" (whnf PrimArray.Traverse.benchmarkPrimMonad PrimArray.Traverse.argument)
        ]
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
