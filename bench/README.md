Benchmark Suite
=======================

The benchmark suite for `primitive` cannot be included in the same package
as `primitive` itself. The benchmark suite depends on `gauge`, which
transitively depends on `primitive`. To break up this dependency cycle,
the test suite lives here in its own unpublished package.
