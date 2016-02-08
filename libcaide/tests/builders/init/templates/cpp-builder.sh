#!/bin/bash

# exit on error
set -e

ROOT=$("$CAIDE" printRoot)
PROB=$("$CAIDE" getstate core problem)

cd "$ROOT/$PROB"
"$CXX" "$PROB.cpp" "${PROB}_test.cpp" -I../cpplib -o "$PROB".exe

cd "$ROOT/$PROB/.caideproblem/test"
../../"$PROB.exe"

