@echo off

:: Setup environment variables required for build.
:: See INSTALL.txt for details.

for %%X in (cabal.exe) do (set HAVE_CABAL=%%~$PATH:X)
for %%X in (ghc.exe) do (set GHC_DIR=%%~$PATH:X)

if defined HAVE_CABAL (
    echo Found cabal in %HAVE_CABAL%. Assuming that ghc.exe is in PATH.
    if not defined MSYS_DIR (
        echo MSYS_DIR environment variable not defined.
        goto :eof
    )

    if not exist "%MSYS_DIR%\msys2_shell.cmd" (
        echo MSYS_DIR environment variable doesn't seem to point to a valid msys installation directory.
        goto :eof
    )

    if defined GHC_DIR (
        echo Using ghc in %GHC_DIR%.
        SET GHC_DIR=%GHC_DIR%\..\..
    ) else (
        echo Didn't find ghc in PATH.
        goto :eof
    )
) else (
    echo Didn't find cabal.exe. Assuming that stack.exe is in PATH.
    FOR /F "tokens=* USEBACKQ" %%F IN (`stack path --compiler-bin`) DO (
      SET GHC_DIR=%%F\..
    )
    FOR /F "tokens=* USEBACKQ" %%F IN (`stack exec bash -- -c "mount | head -1 | awk '{print $1}'"`) DO (
      SET MSYS_DIR=%%F\..
    )
)


set PATH=%PATH%;%GHC_DIR%\mingw\bin;%GHC_DIR%\mingw\x86_64-w64-mingw32\bin;%GHC_DIR%\mingw\i686-w64-mingw32\bin
set PATH=%PATH%;%MSYS_DIR%\usr\bin;%MSYS_DIR%\mingw64\bin

if not defined CSC (
    set CSC=c:\Windows\Microsoft.NET\Framework\v4.0.30319\csc.exe
)

if not exist "%CSC%" (
    echo %CSC% not found. Some tests will likely fail.
)

