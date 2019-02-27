#!/bin/bash
set -ev

env

timeit() {
    now=$(date +%s)
    if [ -n "$last" ] ; then
        diff=$(echo "$now - $last" | bc -l)
        echo "Elapsed $diff seconds"
    fi
    last=$now
}

timeit

sudo apt update
timeit

# Install build dependencies
sudo apt install git g++ cmake cabal-install ghc binutils python2.7 ccache
timeit

cmake --version
g++ --version
ghc --version

git submodule update --init --recursive --depth 1
timeit

cd libcaide

# Build Haskell dependencies
cabal sandbox init
cabal update -v
timeit
cabal install --only-dependencies
timeit

# Build
export CC=`pwd`/../.circleci/ccache-gcc
export CXX=`pwd`/../.circleci/ccache-g++
echo $CC
echo $CXX

ccache --print-config
ccache --show-stats

cabal configure
cabal build --ghc-options="-pgml $CXX"
timeit

ccache --show-stats

unset CC
unset CXX

# Install test dependencies
sudo apt install phantomjs mono-mcs wget curl
timeit

# Run tests
export MONO=mono
export CSC=mcs
export QT_QPA_PLATFORM=offscreen
tests/run-tests.sh
timeit

