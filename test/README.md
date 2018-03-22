Test Suite
=======================

The test suite for `primitive` cannot be included in the same package
as `primitive` itself. The test suite depends on `QuickCheck`, which
transitively depends on `primitive`. To break up this dependency cycle,
the test suite lives here in its own unpublished package.

This test suite is tested by travis. Although `primitive` supports
versions of transformers all the way back to `transformers-0.2.0.0`,
the test suite cannot be build with versions of transformers older than
`transformers-0.3.0.0`.  As far as test coverage goes, this should not
be a problem since there is no CPP that treats these two versions of
transformers differently.  Travis tests `transformers-0.3.0.0` with
its GHC 7.8.4 build, which provides high confidence that `primitive`
works with every version of transformers that it claims to.

