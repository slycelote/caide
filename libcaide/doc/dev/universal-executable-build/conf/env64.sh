export LD_LIBRARY_PATH=/usr/local/lib64:/usr/local/lib
unset CABAL_CONFIG
export PATH=~/bin64:~/.ghcup/ghc/8.10.1/bin:"$PATH":~/bin
unset GHC_LINKER_32
export CAIDE_CMAKE_CONFIGURE_ARGS="-G,Ninja"

