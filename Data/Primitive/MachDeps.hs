{-# LANGUAGE CPP, MagicHash #-}

-- |
-- Module      : Data.Primitive.MachDeps
-- Copyright   : (c) Roman Leshchinskiy 2009-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Machine-dependent constants.

module Data.Primitive.MachDeps where

#include "MachDeps.h"

import GHC.Exts

sIZEOF_CHAR,
 aLIGNMENT_CHAR,

 sIZEOF_INT,
 aLIGNMENT_INT,

 sIZEOF_WORD,
 aLIGNMENT_WORD,

 sIZEOF_DOUBLE,
 aLIGNMENT_DOUBLE,

 sIZEOF_FLOAT,
 aLIGNMENT_FLOAT,

 sIZEOF_PTR,
 aLIGNMENT_PTR,

 sIZEOF_FUNPTR,
 aLIGNMENT_FUNPTR,

 sIZEOF_STABLEPTR,
 aLIGNMENT_STABLEPTR,

 sIZEOF_INT8,
 aLIGNMENT_INT8,

 sIZEOF_WORD8,
 aLIGNMENT_WORD8,

 sIZEOF_INT16,
 aLIGNMENT_INT16,

 sIZEOF_WORD16,
 aLIGNMENT_WORD16,

 sIZEOF_INT32,
 aLIGNMENT_INT32,

 sIZEOF_WORD32,
 aLIGNMENT_WORD32,

 sIZEOF_INT64,
 aLIGNMENT_INT64,

 sIZEOF_WORD64,
 aLIGNMENT_WORD64 :: Int


sIZEOF_CHAR = SIZEOF_HSCHAR
aLIGNMENT_CHAR = ALIGNMENT_HSCHAR

sIZEOF_INT = SIZEOF_HSINT
aLIGNMENT_INT = ALIGNMENT_HSINT

sIZEOF_WORD = SIZEOF_HSWORD
aLIGNMENT_WORD = ALIGNMENT_HSWORD

sIZEOF_DOUBLE = SIZEOF_HSDOUBLE
aLIGNMENT_DOUBLE = ALIGNMENT_HSDOUBLE

sIZEOF_FLOAT = SIZEOF_HSFLOAT
aLIGNMENT_FLOAT = ALIGNMENT_HSFLOAT

sIZEOF_PTR = SIZEOF_HSPTR
aLIGNMENT_PTR = ALIGNMENT_HSPTR

sIZEOF_FUNPTR = SIZEOF_HSFUNPTR
aLIGNMENT_FUNPTR = ALIGNMENT_HSFUNPTR

sIZEOF_STABLEPTR = SIZEOF_HSSTABLEPTR
aLIGNMENT_STABLEPTR = ALIGNMENT_HSSTABLEPTR

sIZEOF_INT8 = SIZEOF_INT8
aLIGNMENT_INT8 = ALIGNMENT_INT8

sIZEOF_WORD8 = SIZEOF_WORD8
aLIGNMENT_WORD8 = ALIGNMENT_WORD8

sIZEOF_INT16 = SIZEOF_INT16
aLIGNMENT_INT16 = ALIGNMENT_INT16

sIZEOF_WORD16 = SIZEOF_WORD16
aLIGNMENT_WORD16 = ALIGNMENT_WORD16

sIZEOF_INT32 = SIZEOF_INT32
aLIGNMENT_INT32 = ALIGNMENT_INT32

sIZEOF_WORD32 = SIZEOF_WORD32
aLIGNMENT_WORD32 = ALIGNMENT_WORD32

sIZEOF_INT64 = SIZEOF_INT64
aLIGNMENT_INT64 = ALIGNMENT_INT64

sIZEOF_WORD64 = SIZEOF_WORD64
aLIGNMENT_WORD64 = ALIGNMENT_WORD64

#if WORD_SIZE_IN_BITS == 32 || __GLASGOW_HASKELL__ >= 903
type Word64_# = Word64#
type Int64_# = Int64#
#else
type Word64_# = Word#
type Int64_# = Int#
#endif
