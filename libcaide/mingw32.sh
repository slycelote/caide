# This script sets up development enviroment in Windows.
# Replace the paths according to your system's configuration.
HPPATH=/c/bin/HaskellPlatform/with-new-mingw
PATH="/c/bin/cabal/.cabal-sandbox/bin:/c/python27:$HPPATH/bin:$HPPATH/lib/extralibs/bin:$HPPATH/mingw/bin:/c/bin/mingw/bin:$PATH"
export CSC=/c/Windows/Microsoft.NET/Framework/v4.0.30319/csc.exe
