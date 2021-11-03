#!/bin/bash

# To use this builder, set CAIDE variable below to full path to caide executable
# and add the following section to caide.ini file:
# [csharp]
# build_and_run_tests: bash /full/path/to/this/file.sh
# evaluates_tests: yes


# exit on error
set -e

# path to caide executable
CAIDE=caide


ROOT=$($CAIDE printRoot)
PROB=$($CAIDE getstate core problem)

cd "$ROOT/$PROB"
gmcs /out:"$PROB".exe "$PROB.cs" "${PROB}_test.cs"

"./$PROB.exe"

