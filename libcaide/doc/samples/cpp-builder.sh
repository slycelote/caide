#!/bin/bash

# To use this builder, set CAIDE variable below to full path to caide executable
# and add the following section to caide.ini file:
# [cpp]
# build_and_run_tests: bash /full/path/to/this/file.sh

# exit on error
set -e

# path to caide executable
CAIDE=caide

ROOT=$($CAIDE printRoot)
PROB=$($CAIDE getstate core problem)

cd "$ROOT/$PROB"
g++ "$PROB.cpp" "${PROB}_test.cpp" -I../cpplib -o "$PROB".exe
"./$PROB.exe"

