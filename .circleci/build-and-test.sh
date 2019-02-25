#!/bin/bash
set -ev

env
date

sudo apt update

# Install build dependencies
sudo apt install git g++ cmake cabal-install ghc binutils python2.7 ccache

cmake --version
g++ --version
ghc --version
date

git submodule update --init --recursive
date

cd libcaide

# Build Haskell dependencies
cabal sandbox init
cabal update -v
cabal install --only-dependencies
date

# Build
export CC=`pwd`/../.circleci/ccache-gcc
export CXX=`pwd`/../.circleci/ccache-g++
echo $CC
echo $CXX

ccache --print-config
ccache --show-stats

cabal configure
cabal build --ghc-options="-pgml $CXX"
date

ccache --show-stats

unset CC
unset CXX

# Install test dependencies
sudo apt install phantomjs mono-mcs wget curl
date

# Run tests
export MONO=mono
export CSC=mcs
export QT_QPA_PLATFORM=offscreen
tests/run-tests.sh
date

