#!/bin/bash

# This script (not a builder) runs current solution in gdb

set -e

# path to caide executable
CAIDE=caide

ROOT=$($CAIDE printRoot)
PROB=$($CAIDE getstate core problem)

cd "$ROOT/$PROB/.caideproblem/test"
gdb ../../"$PROB.exe"

