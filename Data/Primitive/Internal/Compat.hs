-- |
-- Module      : Data.Primitive.Internal.Compat
-- Copyright   : (c) Roman Leshchinskiy 2011-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
-- 
-- Compatibility functions
--

module Data.Primitive.Internal.Compat (mkNoRepType) where

#if MIN_VERSION_base(4,2,0)
import Data.Data (mkNoRepType)
#else
import Data.Data (mkNorepType)

mkNoRepType = mkNorepType
#endif

