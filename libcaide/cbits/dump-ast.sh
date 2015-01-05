#!/bin/sh
clangbuild/out/bin/clang++ -cc1 -ast-dump -isystem ../res/init/include/ -isystem ../res/init/include/mingw-4.8.1/ -isystem ../res/init/include/mingw-4.8.1/c++ -isystem ../res/init/include/ mingw-4.8.1/c++/mingw32/ -std=c++11 $*

