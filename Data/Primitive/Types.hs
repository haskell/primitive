{-# LANGUAGE CPP, UnboxedTuples, MagicHash, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TypeInType #-}
#endif

#include "HsBaseConfig.h"

-- |
-- Module      : Data.Primitive.Types
-- Copyright   : (c) Roman Leshchinskiy 2009-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Basic types and classes for primitive array operations
--

module Data.Primitive.Types (
  Prim(..),
  sizeOf, alignment,

  Addr(..),
) where

import Control.Monad.Primitive
import Data.Primitive.MachDeps
import Data.Primitive.Internal.Operations
import Foreign.C.Types
import System.Posix.Types

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

import GHC.Ptr (
    Ptr(..), FunPtr(..)
  )

import GHC.Prim
#if __GLASGOW_HASKELL__ >= 706
    hiding (setByteArray#)
#endif

import Data.Typeable ( Typeable )
import Data.Data ( Data(..) )
import Data.Primitive.Internal.Compat ( isTrue#, mkNoRepType )
import Numeric

-- | A machine address
data Addr = Addr Addr# deriving ( Typeable )

instance Show Addr where
  showsPrec _ (Addr a) =
    showString "0x" . showHex (fromIntegral (I# (addr2Int# a)) :: Word)

instance Eq Addr where
  Addr a# == Addr b# = isTrue# (eqAddr# a# b#)
  Addr a# /= Addr b# = isTrue# (neAddr# a# b#)

instance Ord Addr where
  Addr a# > Addr b# = isTrue# (gtAddr# a# b#)
  Addr a# >= Addr b# = isTrue# (geAddr# a# b#)
  Addr a# < Addr b# = isTrue# (ltAddr# a# b#)
  Addr a# <= Addr b# = isTrue# (leAddr# a# b#)

instance Data Addr where
  toConstr _ = error "toConstr"
  gunfold _ _ = error "gunfold"
  dataTypeOf _ = mkNoRepType "Data.Primitive.Types.Addr"


-- | Class of types supporting primitive array operations
class Prim a where

  -- | Size of values of type @a@. The argument is not used.
  sizeOf#    :: a -> Int#

  -- | Alignment of values of type @a@. The argument is not used.
  alignment# :: a -> Int#

  -- | Read a value from the array. The offset is in elements of type
  -- @a@ rather than in bytes.
  indexByteArray# :: ByteArray# -> Int# -> a

  -- | Read a value from the mutable array. The offset is in elements of type
  -- @a@ rather than in bytes.
  readByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)

  -- | Write a value to the mutable array. The offset is in elements of type
  -- @a@ rather than in bytes.
  writeByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s

  -- | Fill a slice of the mutable array with a value. The offset and length
  -- of the chunk are in elements of type @a@ rather than in bytes.
  setByteArray# :: MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s

  -- | Read a value from a memory position given by an address and an offset.
  -- The memory block the address refers to must be immutable. The offset is in
  -- elements of type @a@ rather than in bytes.
  indexOffAddr# :: Addr# -> Int# -> a

  -- | Read a value from a memory position given by an address and an offset.
  -- The offset is in elements of type @a@ rather than in bytes.
  readOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, a #)

  -- | Write a value to a memory position given by an address and an offset.
  -- The offset is in elements of type @a@ rather than in bytes.
  writeOffAddr# :: Addr# -> Int# -> a -> State# s -> State# s

  -- | Fill a memory block given by an address, an offset and a length.
  -- The offset and length are in elements of type @a@ rather than in bytes.
  setOffAddr# :: Addr# -> Int# -> Int# -> a -> State# s -> State# s

-- | Size of values of type @a@. The argument is not used.
sizeOf :: Prim a => a -> Int
sizeOf x = I# (sizeOf# x)

-- | Alignment of values of type @a@. The argument is not used.
alignment :: Prim a => a -> Int
alignment x = I# (alignment# x)

#define derivePrim(ty, ctr, sz, align, idx_arr, rd_arr, wr_arr, set_arr, idx_addr, rd_addr, wr_addr, set_addr) \
instance Prim (ty) where {                                      \
  sizeOf# _ = unI# sz                                           \
; alignment# _ = unI# align                                     \
; indexByteArray# arr# i# = ctr (idx_arr arr# i#)               \
; readByteArray#  arr# i# s# = case rd_arr arr# i# s# of        \
                        { (# s1#, x# #) -> (# s1#, ctr x# #) }  \
; writeByteArray# arr# i# (ctr x#) s# = wr_arr arr# i# x# s#    \
; setByteArray# arr# i# n# (ctr x#) s#                          \
    = let { i = fromIntegral (I# i#)                            \
          ; n = fromIntegral (I# n#)                            \
          } in                                                  \
      case unsafeCoerce# (internal (set_arr arr# i n x#)) s# of \
        { (# s1#, _ #) -> s1# }                                 \
                                                                \
; indexOffAddr# addr# i# = ctr (idx_addr addr# i#)              \
; readOffAddr#  addr# i# s# = case rd_addr addr# i# s# of       \
                        { (# s1#, x# #) -> (# s1#, ctr x# #) }  \
; writeOffAddr# addr# i# (ctr x#) s# = wr_addr addr# i# x# s#   \
; setOffAddr# addr# i# n# (ctr x#) s#                           \
    = let { i = fromIntegral (I# i#)                            \
          ; n = fromIntegral (I# n#)                            \
          } in                                                  \
      case unsafeCoerce# (internal (set_addr addr# i n x#)) s# of \
        { (# s1#, _ #) -> s1# }                                 \
; {-# INLINE sizeOf# #-}                                        \
; {-# INLINE alignment# #-}                                     \
; {-# INLINE indexByteArray# #-}                                \
; {-# INLINE readByteArray# #-}                                 \
; {-# INLINE writeByteArray# #-}                                \
; {-# INLINE setByteArray# #-}                                  \
; {-# INLINE indexOffAddr# #-}                                  \
; {-# INLINE readOffAddr# #-}                                   \
; {-# INLINE writeOffAddr# #-}                                  \
; {-# INLINE setOffAddr# #-}                                    \
}

unI# :: Int -> Int#
unI# (I# n#) = n#

derivePrim(Word, W#, sIZEOF_WORD, aLIGNMENT_WORD,
           indexWordArray#, readWordArray#, writeWordArray#, setWordArray#,
           indexWordOffAddr#, readWordOffAddr#, writeWordOffAddr#, setWordOffAddr#)
derivePrim(Word8, W8#, sIZEOF_WORD8, aLIGNMENT_WORD8,
           indexWord8Array#, readWord8Array#, writeWord8Array#, setWord8Array#,
           indexWord8OffAddr#, readWord8OffAddr#, writeWord8OffAddr#, setWord8OffAddr#)
derivePrim(Word16, W16#, sIZEOF_WORD16, aLIGNMENT_WORD16,
           indexWord16Array#, readWord16Array#, writeWord16Array#, setWord16Array#,
           indexWord16OffAddr#, readWord16OffAddr#, writeWord16OffAddr#, setWord16OffAddr#)
derivePrim(Word32, W32#, sIZEOF_WORD32, aLIGNMENT_WORD32,
           indexWord32Array#, readWord32Array#, writeWord32Array#, setWord32Array#,
           indexWord32OffAddr#, readWord32OffAddr#, writeWord32OffAddr#, setWord32OffAddr#)
derivePrim(Word64, W64#, sIZEOF_WORD64, aLIGNMENT_WORD64,
           indexWord64Array#, readWord64Array#, writeWord64Array#, setWord64Array#,
           indexWord64OffAddr#, readWord64OffAddr#, writeWord64OffAddr#, setWord64OffAddr#)
derivePrim(Int, I#, sIZEOF_INT, aLIGNMENT_INT,
           indexIntArray#, readIntArray#, writeIntArray#, setIntArray#,
           indexIntOffAddr#, readIntOffAddr#, writeIntOffAddr#, setIntOffAddr#)
derivePrim(Int8, I8#, sIZEOF_INT8, aLIGNMENT_INT8,
           indexInt8Array#, readInt8Array#, writeInt8Array#, setInt8Array#,
           indexInt8OffAddr#, readInt8OffAddr#, writeInt8OffAddr#, setInt8OffAddr#)
derivePrim(Int16, I16#, sIZEOF_INT16, aLIGNMENT_INT16,
           indexInt16Array#, readInt16Array#, writeInt16Array#, setInt16Array#,
           indexInt16OffAddr#, readInt16OffAddr#, writeInt16OffAddr#, setInt16OffAddr#)
derivePrim(Int32, I32#, sIZEOF_INT32, aLIGNMENT_INT32,
           indexInt32Array#, readInt32Array#, writeInt32Array#, setInt32Array#,
           indexInt32OffAddr#, readInt32OffAddr#, writeInt32OffAddr#, setInt32OffAddr#)
derivePrim(Int64, I64#, sIZEOF_INT64, aLIGNMENT_INT64,
           indexInt64Array#, readInt64Array#, writeInt64Array#, setInt64Array#,
           indexInt64OffAddr#, readInt64OffAddr#, writeInt64OffAddr#, setInt64OffAddr#)
derivePrim(Float, F#, sIZEOF_FLOAT, aLIGNMENT_FLOAT,
           indexFloatArray#, readFloatArray#, writeFloatArray#, setFloatArray#,
           indexFloatOffAddr#, readFloatOffAddr#, writeFloatOffAddr#, setFloatOffAddr#)
derivePrim(Double, D#, sIZEOF_DOUBLE, aLIGNMENT_DOUBLE,
           indexDoubleArray#, readDoubleArray#, writeDoubleArray#, setDoubleArray#,
           indexDoubleOffAddr#, readDoubleOffAddr#, writeDoubleOffAddr#, setDoubleOffAddr#)
derivePrim(Char, C#, sIZEOF_CHAR, aLIGNMENT_CHAR,
           indexWideCharArray#, readWideCharArray#, writeWideCharArray#, setWideCharArray#,
           indexWideCharOffAddr#, readWideCharOffAddr#, writeWideCharOffAddr#, setWideCharOffAddr#)
derivePrim(Addr, Addr, sIZEOF_PTR, aLIGNMENT_PTR,
           indexAddrArray#, readAddrArray#, writeAddrArray#, setAddrArray#,
           indexAddrOffAddr#, readAddrOffAddr#, writeAddrOffAddr#, setAddrOffAddr#)
derivePrim(Ptr a, Ptr, sIZEOF_PTR, aLIGNMENT_PTR,
           indexAddrArray#, readAddrArray#, writeAddrArray#, setAddrArray#,
           indexAddrOffAddr#, readAddrOffAddr#, writeAddrOffAddr#, setAddrOffAddr#)
derivePrim(FunPtr a, FunPtr, sIZEOF_PTR, aLIGNMENT_PTR,
           indexAddrArray#, readAddrArray#, writeAddrArray#, setAddrArray#,
           indexAddrOffAddr#, readAddrOffAddr#, writeAddrOffAddr#, setAddrOffAddr#)

-- Prim instances for newtypes in Foreign.C.Types
deriving instance Prim CChar
deriving instance Prim CSChar
deriving instance Prim CUChar
deriving instance Prim CShort
deriving instance Prim CUShort
deriving instance Prim CInt
deriving instance Prim CUInt
deriving instance Prim CLong
deriving instance Prim CULong
deriving instance Prim CPtrdiff
deriving instance Prim CSize
deriving instance Prim CWchar
deriving instance Prim CSigAtomic
deriving instance Prim CLLong
deriving instance Prim CULLong
#if MIN_VERSION_base(4,10,0)
deriving instance Prim CBool
#endif
deriving instance Prim CIntPtr
deriving instance Prim CUIntPtr
deriving instance Prim CIntMax
deriving instance Prim CUIntMax
deriving instance Prim CClock
deriving instance Prim CTime
deriving instance Prim CUSeconds
deriving instance Prim CSUSeconds
deriving instance Prim CFloat
deriving instance Prim CDouble

-- Prim instances for newtypes in System.Posix.Types
#if defined(HTYPE_DEV_T)
deriving instance Prim CDev
#endif
#if defined(HTYPE_INO_T)
deriving instance Prim CIno
#endif
#if defined(HTYPE_MODE_T)
deriving instance Prim CMode
#endif
#if defined(HTYPE_OFF_T)
deriving instance Prim COff
#endif
#if defined(HTYPE_PID_T)
deriving instance Prim CPid
#endif
#if defined(HTYPE_SSIZE_T)
deriving instance Prim CSsize
#endif
#if defined(HTYPE_GID_T)
deriving instance Prim CGid
#endif
#if defined(HTYPE_NLINK_T)
deriving instance Prim CNlink
#endif
#if defined(HTYPE_UID_T)
deriving instance Prim CUid
#endif
#if defined(HTYPE_CC_T)
deriving instance Prim CCc
#endif
#if defined(HTYPE_SPEED_T)
deriving instance Prim CSpeed
#endif
#if defined(HTYPE_TCFLAG_T)
deriving instance Prim CTcflag
#endif
#if defined(HTYPE_RLIM_T)
deriving instance Prim CRLim
#endif
#if defined(HTYPE_BLKSIZE_T)
deriving instance Prim CBlkSize
#endif
#if defined(HTYPE_BLKCNT_T)
deriving instance Prim CBlkCnt
#endif
#if defined(HTYPE_CLOCKID_T)
deriving instance Prim CClockId
#endif
#if defined(HTYPE_FSBLKCNT_T)
deriving instance Prim CFsBlkCnt
#endif
#if defined(HTYPE_FSFILCNT_T)
deriving instance Prim CFsFilCnt
#endif
#if defined(HTYPE_ID_T)
deriving instance Prim CId
#endif
#if defined(HTYPE_KEY_T)
deriving instance Prim CKey
#endif
#if defined(HTYPE_TIMER_T)
deriving instance Prim CTimer
#endif
deriving instance Prim Fd
