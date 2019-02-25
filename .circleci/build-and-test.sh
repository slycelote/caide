#!/bin/bash
set -ev

env
date

sudo apt update
sudo apt install git g++ cmake cabal-install ghc binutils python2.7 ccache

cmake --version
g++ --version
ghc --version
date

git submodule update --init --recursive
date

cd libcaide
cabal sandbox init
cabal update -v
cabal install --only-dependencies
date

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

sudo apt install phantomjs mono-mcs wget curl
date

export MONO=mono
export CSC=mcs
export QT_QPA_PLATFORM=offscreen
tests/run-tests.sh
date

