Identifiers - Int ids for values.
=======================================

Example:
--------

    > import Data.Identifiers
    > let myIds = fromList ["foo", "bar", "baz"]
    > lookupId myIds "baz"
    Just 2
    > lookupKey myIds 2
    Just "baz"

