## Changes in version 0.6.4.0

 * Add `Eq1`, `Ord1`, `Show1`, and `Read1` instances for `Array` and
   `SmallArray`.

 * Improve the test suite. This includes having property tests for
   typeclasses from `base` such as `Eq`, `Ord`, `Functor`, `Applicative`,
   `Monad`, `IsList`, `Monoid`, `Foldable`, and `Traversable`.

 * Fix the broken `IsList` instance for `ByteArray`.

 * Fix the broken `Functor`, `Applicative`, and `Monad` instances for
   `Array` and `SmallArray`.

 * Add `PrimUnlifted` instances for `TVar` and `MVar`.

## Changes in version 0.6.3.0

 * Add `PrimMonad` instances for `ContT`, `AccumT`, and `SelectT` from
   `transformers`

 * Add `Eq`, `Ord`, `Show`, and `IsList` instances for `ByteArray`

 * Add `Semigroup` instances for `Array` and `SmallArray`. This allows
   `primitive` to build on GHC 8.4 and later.

## Changes in version 0.6.2.0

 * Drop support for GHCs before 7.4

 * `SmallArray` support

 * `ArrayArray#` based support for more efficient arrays of unlifted pointer types

 * Make `Array` and the like instances of various classes for convenient use

 * Add `Prim` instances for Ptr and FunPtr

 * Add `ioToPrim`, `stToPrim` and unsafe counterparts for situations that would
   otherwise require type ascriptions on `primToPrim`

 * Add `evalPrim`

 * Add `PrimBase` instance for `IdentityT`

## Changes in version 0.6.1.0

 * Use more appropriate types in internal memset functions, which prevents
   overflows/segfaults on 64-bit systems.

 * Fixed a warning on GHC 7.10

 * Worked around a -dcore-lint bug in GHC 7.6/7.7

## Changes in version 0.6

 * Split PrimMonad into two classes to allow automatic lifting of primitive
   operations into monad transformers. The `internal` operation has moved to the
   `PrimBase` class.

 * Fixed the test suite on older GHCs

## Changes in version 0.5.4.0

 * Changed primitive_ to work around an oddity with GHC's code generation
   on certain versions that led to side effects not happening when used
   in conjunction with certain very unsafe IO performers.

 * Allow primitive to build on GHC 7.9

## Changes in version 0.5.3.0

 * Implement `cloneArray` and `cloneMutableArray` primitives
   (with fall-back implementations for GHCs prior to version 7.2.1)

## Changes in version 0.5.2.1

 * Add strict variants of `MutVar` modification functions
   `atomicModifyMutVar'` and `modifyMutVar'`

 * Fix compilation on Solaris 10 with GNU C 3.4.3

## Changes in version 0.5.1.0

 * Add support for GHC 7.7's new primitive `Bool` representation

## Changes in version 0.5.0.1

 * Disable array copying primitives for GHC 7.6.* and earlier

## Changes in version 0.5

 * New in `Data.Primitive.MutVar`: `atomicModifyMutVar`

 * Efficient block fill operations: `setByteArray`, `setAddr`

## Changes in version 0.4.1

 * New module `Data.Primitive.MutVar`

## Changes in version 0.4.0.1

 * Critical bug fix in `fillByteArray`

## Changes in version 0.4

 * Support for GHC 7.2 array copying primitives

 * New in `Data.Primitive.ByteArray`: `copyByteArray`,
   `copyMutableByteArray`, `moveByteArray`, `fillByteArray`

 * Deprecated in `Data.Primitive.ByteArray`: `memcpyByteArray`,
   `memcpyByteArray'`, `memmoveByteArray`, `memsetByteArray`

 * New in `Data.Primitive.Array`: `copyArray`, `copyMutableByteArray`

 * New in `Data.Primitive.Addr`: `copyAddr`, `moveAddr`

 * Deprecated in `Data.Primitive.Addr`: `memcpyAddr`
