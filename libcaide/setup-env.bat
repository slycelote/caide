@echo off

rem Setup environment variables required for build.
rem See INSTALL.txt for details.

FOR /F "tokens=* USEBACKQ" %%F IN (`stack path --compiler-bin`) DO (
  SET CAIDE_GHC=%%F\..
)

FOR /F "tokens=* USEBACKQ" %%F IN (`dir /AD /B "%CAIDE_GHC%\..\msys2*"`) DO (
  SET CAIDE_MSYS=%CAIDE_GHC%\..\%%F\msys2_shell -mingw32 -here
)

SET PATH=%PATH%;%CAIDE_GHC%\mingw\bin;%CAIDE_GHC%\mingw\i686-w64-mingw32\bin

rem Add additional PATH entries if necessary, e.g.:
rem SET PATH=%PATH%;C:\cmake\bin;C:\Python27

