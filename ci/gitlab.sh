#!/bin/bash
set -ev

gcc --version
g++ --version
cmake --version
ghc --version
cabal --version
phantomjs --version
mcs --version

date

cabal update -v
date

cd libcaide
cabal install --only-dependencies
date

cabal configure
cabal build --ghc-options="-pgml g++"
date

export MONO=mono
export CSC=mcs
# For phantomjs
export QT_QPA_PLATFORM=offscreen
export QT_QPA_FONTDIR=/usr/share/fonts
tests/run-tests.sh
date

