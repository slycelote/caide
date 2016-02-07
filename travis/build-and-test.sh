#!/bin/bash
set -ev

export CXX=g++-4.9
export CC=gcc-4.9

git submodule update --init --recursive
cd libcaide
cabal install --only-dependencies

cabal configure
cabal build --ghc-options="-pgml $CXX"

tests/run-tests.sh

