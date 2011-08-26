module Data.Primitive.Internal.Compat (mkNoRepType) where

#if MIN_VERSION_base(4,2,0)
import Data.Data (mkNoRepType)
#else
import Data.Data (mkNorepType)

mkNoRepType = mkNorepType
#endif

