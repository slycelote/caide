@echo off
:: The following is a way to sleep for CAIDE_TEST_BUILD_TIME seconds
:: timeout command doesn't work with redirected stdin
:: ping doesn't exit on ctrl-break so we're using a loop.
setlocal
set i=0

:loop
if %i% equ %CAIDE_TEST_BUILD_TIME% (goto :done)
ping 192.0.2.1 -w 1000 -n 2 >nul
set /a i+=1
goto :loop

:done
exit /b %CAIDE_TEST_BUILD_EXIT_CODE%

