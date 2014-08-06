{-# LANGUAGE MagicHash, UnboxedTuples #-}
import Control.Monad.Primitive
import Data.Primitive.Array
import GHC.IO
import GHC.Prim
import System.IO.Unsafe

main :: IO ()
main = do
    arr <- newArray 1 'A'
    let unit = unsafePerformIO $ writeArray arr 0 'B'
    c1 <- readArray arr 0
    return $! unit
    c2 <- readArray arr 0
    if c1 == 'A' && c2 == 'B'
        then return ()
        else error $ "Expected AB, got: " ++ show (c1, c2)
