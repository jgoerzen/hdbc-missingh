Welcome to HDBC-MissingH.

HDBC-MissingH is a package to integrate HDBC (Haskell Database
Connectivity) with the MissingH library.

At the moment, this means that any HDBC database can act as a backend
for the MissingH.AnyDBM interface.

Please see doc/MissingH-AnyDBM-HDBCDBM.html for more information.

PREREQUISITES
-------------

1) You'll need either GHC 6.4.1 or above, or Hugs 2005xx or above.

2) Also, you need MissingH 0.13.0 or above.  Download it from
   http://quux.org/devel/missingh

3) Finally, you need HDBC 0.99.0 or above.  Download it from
   http://quux.org/devel/hdbc

INSTALLATION
------------

The steps to install are:

1) ghc --make -o setup Setup.lhs

2) ./setup configure

3) ./setup build

4) ./setup install   (as root)

If you're on Windows, you can omit the leading "./".

Documentation is in doc/.

USAGE
-----

To use with hugs, you'll want to use hugs -98.

To use with GHC, you'll want to use -package HDBC-MissingH in your programs.
Or, with Cabal, use Build-Depends: HDBC-MissingH.

-- John Goerzen
   December 2005
