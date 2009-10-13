{-# LANGUAGE UnboxedTuples, MagicHash, CPP #-}

-- |
-- Module      : Data.Primitive.Types
-- Copyright   : (c) Roman Leshchinskiy 2009
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
-- 
-- Basic types and classes for primitive array operations
--

module Data.Primitive.Types (
  Addr(..),
  Prim(..), sizeOf, alignment
) where

import Control.Monad.Primitive
import Data.Primitive.MachDeps

import GHC.Base (
    Int(..), Char(..),
  )
import GHC.Float (
    Float(..), Double(..)
  )
import GHC.Word (
    Word(..), Word8(..), Word16(..), Word32(..), Word64(..)
  )
import GHC.Int (
    Int8(..), Int16(..), Int32(..), Int64(..)
  )

import GHC.Prim

data Addr = Addr Addr#

class Prim a where
  sizeOf#    :: a -> Int#
  alignment# :: a -> Int#

  indexByteArray# :: ByteArray# -> Int# -> a
  readByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  writeByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s

  indexOffAddr# :: Addr# -> Int# -> a
  readOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, a #)
  writeOffAddr# :: Addr# -> Int# -> a -> State# s -> State# s

sizeOf :: Prim a => a -> Int
sizeOf x = I# (sizeOf# x)

alignment :: Prim a => a -> Int
alignment x = I# (alignment# x)

#define derivePrim(ty, ctr, sz, align, idx_arr, rd_arr, wr_arr, idx_addr, rd_addr, wr_addr) \
instance Prim ty where {                                        \
  sizeOf# _ = unI# sz                                           \
; alignment# _ = unI# align                                     \
; indexByteArray# arr# i# = ctr (idx_arr arr# i#)               \
; readByteArray#  arr# i# s# = case rd_arr arr# i# s# of        \
                        { (# s1#, x# #) -> (# s1#, ctr x# #) }  \
; writeByteArray# arr# i# (ctr x#) s# = wr_arr arr# i# x# s#    \
                                                                \
; indexOffAddr# addr# i# = ctr (idx_addr addr# i#)              \
; readOffAddr#  addr# i# s# = case rd_addr addr# i# s# of       \
                        { (# s1#, x# #) -> (# s1#, ctr x# #) }  \
; writeOffAddr# addr# i# (ctr x#) s# = wr_addr addr# i# x# s#   }

unI# :: Int -> Int#
unI# (I# n#) = n#

derivePrim(Word, W#, sIZEOF_WORD, aLIGNMENT_WORD,
           indexWordArray#, readWordArray#, writeWordArray#,
           indexWordOffAddr#, readWordOffAddr#, writeWordOffAddr#)
derivePrim(Word8, W8#, sIZEOF_WORD8, aLIGNMENT_WORD8,
           indexWord8Array#, readWord8Array#, writeWord8Array#,
           indexWord8OffAddr#, readWord8OffAddr#, writeWord8OffAddr#)
derivePrim(Word16, W16#, sIZEOF_WORD16, aLIGNMENT_WORD16,
           indexWord16Array#, readWord16Array#, writeWord16Array#,
           indexWord16OffAddr#, readWord16OffAddr#, writeWord16OffAddr#)
derivePrim(Word32, W32#, sIZEOF_WORD32, aLIGNMENT_WORD32,
           indexWord32Array#, readWord32Array#, writeWord32Array#,
           indexWord32OffAddr#, readWord32OffAddr#, writeWord32OffAddr#)
derivePrim(Word64, W64#, sIZEOF_WORD64, aLIGNMENT_WORD64,
           indexWord64Array#, readWord64Array#, writeWord64Array#,
           indexWord64OffAddr#, readWord64OffAddr#, writeWord64OffAddr#)
derivePrim(Int, I#, sIZEOF_INT, aLIGNMENT_INT,
           indexIntArray#, readIntArray#, writeIntArray#,
           indexIntOffAddr#, readIntOffAddr#, writeIntOffAddr#)
derivePrim(Int8, I8#, sIZEOF_INT8, aLIGNMENT_INT8,
           indexInt8Array#, readInt8Array#, writeInt8Array#,
           indexInt8OffAddr#, readInt8OffAddr#, writeInt8OffAddr#)
derivePrim(Int16, I16#, sIZEOF_INT16, aLIGNMENT_INT16,
           indexInt16Array#, readInt16Array#, writeInt16Array#,
           indexInt16OffAddr#, readInt16OffAddr#, writeInt16OffAddr#)
derivePrim(Int32, I32#, sIZEOF_INT32, aLIGNMENT_INT32,
           indexInt32Array#, readInt32Array#, writeInt32Array#,
           indexInt32OffAddr#, readInt32OffAddr#, writeInt32OffAddr#)
derivePrim(Int64, I64#, sIZEOF_INT64, aLIGNMENT_INT64,
           indexInt64Array#, readInt64Array#, writeInt64Array#,
           indexInt64OffAddr#, readInt64OffAddr#, writeInt64OffAddr#)
derivePrim(Float, F#, sIZEOF_FLOAT, aLIGNMENT_FLOAT,
           indexFloatArray#, readFloatArray#, writeFloatArray#,
           indexFloatOffAddr#, readFloatOffAddr#, writeFloatOffAddr#)
derivePrim(Double, D#, sIZEOF_DOUBLE, aLIGNMENT_DOUBLE,
           indexDoubleArray#, readDoubleArray#, writeDoubleArray#,
           indexDoubleOffAddr#, readDoubleOffAddr#, writeDoubleOffAddr#)
derivePrim(Char, C#, sIZEOF_CHAR, aLIGNMENT_CHAR,
           indexWideCharArray#, readWideCharArray#, writeWideCharArray#,
           indexWideCharOffAddr#, readWideCharOffAddr#, writeWideCharOffAddr#)
derivePrim(Addr, Addr, sIZEOF_PTR, aLIGNMENT_PTR,
           indexAddrArray#, readAddrArray#, writeAddrArray#,
           indexAddrOffAddr#, readAddrOffAddr#, writeAddrOffAddr#)

