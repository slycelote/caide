#!/bin/bash
set -ev

export QT_QPA_PLATFORM=offscreen

gcc --version
g++ --version
cmake --version
ghc --version
cabal --version
phantomjs --version
mcs --version

date

git submodule update --init --recursive
date

cd libcaide
cabal install --only-dependencies
date

cabal configure
cabal build --ghc-options="-pgml g++"
date

export MONO=mono
tests/run-tests.sh
date

