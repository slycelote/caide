#!/bin/env bash
set -e

# Install 32-bit GHC manually
# See https://gitlab.haskell.org/ghc/ghc/wikis/building/compiling32on64
sudo yum install -y gcc make gmp.i686 libgcc.i686 ncurses-libs.i686 glibc-devel.i686 glibc-devel

cd ghc-8.10.1
CFLAGS=-m32 ./configure --prefix=$HOME/local32
CFLAGS=-m32 make install
cd ..
rm -rf ghc-*

# Install 64-bit GHC and cabal with ghcup
ln -s ~/.ghcup/bin/ghcup bin/ghcup
sudo yum install -y gmp libgcc ncurses-libs glibc-devel
bin/ghcup upgrade
bin/ghcup install 8.10.1
bin/ghcup install-cabal
ln -s ~/.ghcup/bin/cabal bin/cabal

# Additional cabal configuration
bin/cabal --config-file conf/cabalconfig32 user-config init
echo '  hsc2hs-options: --cflag=-m32 --lflag=-m32' >> conf/cabalconfig32

# Build and install newer gcc
sudo yum install -y gcc-c++ gmp-devel mpfr-devel libmpc-devel tar

mkdir gcc-build
cd gcc-build
../gcc-10.1.0/configure --enable-multilib --enable-languages=c,c++ --disable-bootstrap --without-isl
make
sudo make install
cd ..
rm -rf gcc-*

# Remove the old gcc, install the remaining dependencies
sudo yum remove gcc-c++ tar gcc
sudo yum install -y epel-release
sudo yum install -y cmake3 ninja-build zlib-static zlib-static.i686 gmp-static gmp-static.i686 mono-core
ln -s /usr/bin/cmake3 bin/cmake

