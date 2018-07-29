#!/bin/bash

# Setup environment variables required for running tests.
# See INSTALL.txt for details.

export CAIDE_GHC=$(echo /$CAIDE_GHC | sed 's/://g; s|\\|/|g')
export PATH="$PATH:$CAIDE_GHC/mingw/bin:$CAIDE_GHC/mingw/i686-w64-mingw32/bin"

# Change these paths according to your system.
export CSC=/c/Windows/Microsoft.NET/Framework/v4.0.30319/csc.exe
export PHANTOMJS=/c/bin/phantomjs.exe

