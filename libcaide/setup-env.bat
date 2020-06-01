@echo off

rem Setup environment variables required for build.
rem See INSTALL.txt for details.

FOR /F "tokens=* USEBACKQ" %%F IN (`where ghc`) DO (
  SET CAIDE_GHC=%%F\..\..
)

rem If you use Stack, use the following instead:
rem FOR /F "tokens=* USEBACKQ" %%F IN (`stack path --compiler-bin`) DO (
rem   SET CAIDE_GHC=%%F\..
rem )

SET PATH=%PATH%;%CAIDE_GHC%\mingw\bin;%CAIDE_GHC%\mingw\x86_64-w64-mingw32\bin;%CAIDE_GHC%\mingw\i686-w64-mingw32\bin

rem Change these additional PATH entries according to your system if necessary, e.g.:
SET PATH=%PATH%;C:\cmake\bin;C:\Python36

