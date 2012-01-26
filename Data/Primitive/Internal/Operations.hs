{-# LANGUAGE MagicHash, ForeignFunctionInterface, UnliftedFFITypes #-}

-- |
-- Module      : Data.Primitive.Internal.Operations
-- Copyright   : (c) Roman Leshchinskiy 2011
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
-- 
-- Internal operations
--


module Data.Primitive.Internal.Operations (
  setWord8Array#, setWord16Array#, setWord32Array#,
  setWord64Array#, setWordArray#,
  setInt8Array#, setInt16Array#, setInt32Array#,
  setInt64Array#, setIntArray#,
  setAddrArray#, setFloatArray#, setDoubleArray#, setWideCharArray#
) where

import GHC.Prim

foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word8"
  setWord8Array# :: MutableByteArray# s -> Int# -> Int# -> Word# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word16"
  setWord16Array# :: MutableByteArray# s -> Int# -> Int# -> Word# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word32"
  setWord32Array# :: MutableByteArray# s -> Int# -> Int# -> Word# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word64"
  setWord64Array# :: MutableByteArray# s -> Int# -> Int# -> Word64# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word"
  setWordArray# :: MutableByteArray# s -> Int# -> Int# -> Word# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word8"
  setInt8Array# :: MutableByteArray# s -> Int# -> Int# -> Int# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word16"
  setInt16Array# :: MutableByteArray# s -> Int# -> Int# -> Int# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word32"
  setInt32Array# :: MutableByteArray# s -> Int# -> Int# -> Int# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word64"
  setInt64Array# :: MutableByteArray# s -> Int# -> Int# -> Int64# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word"
  setIntArray# :: MutableByteArray# s -> Int# -> Int# -> Int# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Ptr"
  setAddrArray# :: MutableByteArray# s -> Int# -> Int# -> Addr# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Float"
  setFloatArray# :: MutableByteArray# s -> Int# -> Int# -> Float# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Double"
  setDoubleArray# :: MutableByteArray# s -> Int# -> Int# -> Double# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Char"
  setWideCharArray# :: MutableByteArray# s -> Int# -> Int# -> Char# -> IO ()

