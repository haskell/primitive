{-# LANGUAGE BangPatterns, CPP, MagicHash, UnboxedTuples, UnliftedFFITypes, DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- |
-- Module      : Data.Primitive.ByteArray
-- Copyright   : (c) Roman Leshchinskiy 2009-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Primitive operations on byte arrays. Most functions in this module include
-- an element type in their type signature and interpret the unit for offsets
-- and lengths as that element. A few functions (e.g. 'copyByteArray',
-- 'freezeByteArray') do not include an element type. Such functions
-- interpret offsets and lengths as units of 8-bit words.

module Data.Primitive.ByteArray (
  -- * Types
  ByteArray(..), MutableByteArray(..), ByteArray#, MutableByteArray#,

  -- * Allocation
  newByteArray, newPinnedByteArray, newAlignedPinnedByteArray,
  resizeMutableByteArray,
  shrinkMutableByteArray,

  -- * Element access
  readByteArray, writeByteArray, indexByteArray,
  -- * Char Element Access
  -- $charElementAccess
  readCharArray, writeCharArray, indexCharArray,

  -- * Constructing
  emptyByteArray,
  byteArrayFromList, byteArrayFromListN,

  -- * Folding
  foldrByteArray,

  -- * Comparing
  compareByteArrays,

  -- * Freezing and thawing
  freezeByteArray, thawByteArray, runByteArray,
  unsafeFreezeByteArray, unsafeThawByteArray,

  -- * Block operations
  copyByteArray, copyMutableByteArray,
  copyByteArrayToPtr, copyMutableByteArrayToPtr,
  copyByteArrayToAddr, copyMutableByteArrayToAddr,
  copyPtrToMutableByteArray,
  moveByteArray,
  setByteArray, fillByteArray,
  cloneByteArray, cloneMutableByteArray,

  -- * Information
  sizeofByteArray,
  sizeofMutableByteArray, getSizeofMutableByteArray, sameMutableByteArray,
#if __GLASGOW_HASKELL__ >= 802
  isByteArrayPinned, isMutableByteArrayPinned,
#endif
  byteArrayContents, mutableByteArrayContents

) where

import Data.Primitive.ByteArray.Internal hiding
  ( readByteArray, writeByteArray, indexByteArray
  , readCharArray, writeCharArray, indexCharArray
  )
import Data.Primitive.ByteArray.Operations

{- $charElementAccess
GHC provides two sets of element accessors for 'Char'. One set faithfully
represents 'Char' as 32-bit words using UTF-32. The other set represents
'Char' as 8-bit words using Latin-1 (ISO-8859-1), and the write operation
has undefined behavior for codepoints outside of the ASCII and Latin-1
blocks. The 'Prim' instance for 'Char' uses the UTF-32 set of operators.
-}
