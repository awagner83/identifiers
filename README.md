Identifiers - Ids for values.
=============================

Installation:
-------------

    cabal install

Running Tests
-------------

    cabal test

Running Benchmarks
------------------

    cabal bench

Example:
--------

    > import Data.Identifiers
    > let myIds = fromList ["foo", "bar", "baz"] :: Identifiers Int String
    > lookupId myIds "baz"
    Just 2
    > lookupKey myIds 2
    Just "baz"

