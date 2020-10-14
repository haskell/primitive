{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Gauge
import Control.Monad.ST
import Data.Primitive
import Control.DeepSeq
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except

-- These are fixed implementations of certain operations. In the event
-- that primitive changes its implementation of a function, these
-- implementations stay the same. They are helpful for ensuring that
-- something that is a performance win in one version of GHC doesn't
-- become a regression later. They are also helpful for evaluating
-- how well different implementation hold up in different scenarios.
import qualified Array.Traverse.Unsafe
import qualified Array.Traverse.Closure
import qualified Array.Traverse.Either

-- These are particular scenarios that are tested against the
-- implementations actually used by primitive.
import qualified ByteArray.Compare
import qualified PrimArray.Compare
import qualified PrimArray.Traverse

main :: IO ()
main = defaultMain
  [ bgroup "Array"
    [ bgroup "implementations"
      [ bgroup "traverse"
        [ bgroup "general"
          [ bench "closure" (nf (\x -> runST (runStateT (Array.Traverse.Closure.traversePoly cheap x) 0)) numbers)
          , bench "unsafe" (nf (\x -> runST (runStateT (Array.Traverse.Unsafe.traversePoly cheap x) 0)) numbers)
          ]
        , bgroup "Either"
          [ bench "ExceptT"
              ( nf
                ( either id (flip indexArray 0)
                . (\xs -> runST (runExceptT (Array.Traverse.Unsafe.traversePoly (ExceptT . return . (\x -> if x < 0 then Left 0 else Right x)) xs)))
                )
                numbers
              )
          , bench "inlined" (nf (either id (flip indexArray 0) . Array.Traverse.Either.traverseEither (\x -> if x < 0 then Left 0 else Right x)) numbers)
          , bench "closure" (nf (either id (flip indexArray 0) . Array.Traverse.Closure.traversePoly (\x -> if x < 0 then Left 0 else Right x)) numbers)
          ]
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
    , bgroup "implementations"
      [ bgroup "less-than"
        [ bench "default" (whnf (PrimArray.Compare.benchmarkLtDef PrimArray.Compare.argumentA) PrimArray.Compare.argumentB)
        , bench "override" (whnf (PrimArray.Compare.benchmarkLt PrimArray.Compare.argumentA) PrimArray.Compare.argumentB)
        ]
      , bgroup "less-than-equal"
        [ bench "default" (whnf (PrimArray.Compare.benchmarkLteDef PrimArray.Compare.argumentA) PrimArray.Compare.argumentB)
        , bench "override" (whnf (PrimArray.Compare.benchmarkLte PrimArray.Compare.argumentA) PrimArray.Compare.argumentB)
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
