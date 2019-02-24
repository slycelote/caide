#!/bin/bash
set -ev

date

sudo apt update
sudo apt install g++ cmake cabal-install ghc binutils python2.7

cmake --version
g++ --version
ghc --version
date

export CXX=g++

git submodule update --init --recursive
date

cd libcaide
cabal sandbox init
cabal update -v
cabal install --only-dependencies
date

cabal configure
cabal build --ghc-options="-pgml $CXX"
date

sudo apt install phantomjs mono-mcs wget curl
date

export MONO=mono
export CSC=mcs
export QT_QPA_PLATFORM=offscreen
tests/run-tests.sh
date

