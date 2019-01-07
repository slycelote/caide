#!/bin/bash
set -ev

# For phantomjs
export QT_QPA_PLATFORM=offscreen
export QT_QPA_FONTDIR=/usr/share/fonts

gcc --version
g++ --version
cmake --version
ghc --version
cabal --version
phantomjs --version
mcs --version
# Debugging https://github.com/snoyberg/http-client/issues/292
cat /etc/hosts

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
# tests/run-tests.sh
date

