#!/bin/bash
set -ev

date

pwd
ls

sudo apt update
sudo apt install git g++ cmake cabal-install ghc binutils python2.7 ccache

git status


cmake --version
g++ --version
ghc --version
date

git submodule update --init --recursive
date

env
file $CIRCLE_WORKING_DIRECTORY/.circleci/ccache-g++
readlink -f $CIRCLE_WORKING_DIRECTORY/.circleci/ccache-g++

cd libcaide
cabal sandbox init
cabal update -v
cabal install --only-dependencies
date

export CXX=`readlink -f $CIRCLE_WORKING_DIRECTORY/.circleci/ccache-g++`
echo $CXX

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

