#!/bin/bash

# exit on error
set -e


ROOT=$("$CAIDE" printRoot)
PROB=$("$CAIDE" getstate core problem)

cd "$ROOT/$PROB"
"$CSC" /out:"${PROB}cs".exe "$PROB.cs" "${PROB}_test.cs"

if [ "x$MONO" = "x" ]
then "./${PROB}cs.exe"
else "$MONO" "./${PROB}cs.exe"
fi

