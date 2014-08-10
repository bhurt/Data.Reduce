Data.Reduce
===========

Balanced Reduce implementation for Haskell

How to compile
==============

Of course you have ghc and cabal installed, and have git cloned the
repository.  To build it, do:

    cabal sandbox init
    cabal install --only-dependencies
    cabal build

To run the test suite:

    cabal configure --enable-tests
    cabal build
    cabal test
