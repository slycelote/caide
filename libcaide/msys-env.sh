# This script sets up development enviroment in Windows for MSYS shell.
# Replace the paths according to your system's configuration.
HPPATH=/c/bin/HaskellPlatform/7.10.3
export PATH="/c/python27:/c/bin/msys-w64/bin:/c/bin/mingw-w64/mingw32/bin:$HPPATH/bin:$HPPATH/lib/extralibs/bin:/c/bin/CMake/bin:$PATH"
export CSC=/c/Windows/Microsoft.NET/Framework/v4.0.30319/csc.exe

