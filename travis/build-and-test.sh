#!/bin/bash
set -ev

date

export CXX=g++-4.9
export CC=gcc-4.9

git submodule update --init --recursive
date

cd libcaide
cabal install --only-dependencies
date

cabal configure
cabal build --ghc-options="-pgml $CXX"
date

export MONO=mono
tests/run-tests.sh
date

