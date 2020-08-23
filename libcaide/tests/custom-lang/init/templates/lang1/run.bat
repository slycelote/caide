@echo off


setlocal
set i=0

:loop
if %i% equ %CAIDE_TEST_RUN_TIME% (goto :done)
ping 192.0.2.1 -w 1000 -n 2 >nul
set /a i+=1
goto :loop

:done
echo 1 2 3
exit /b %CAIDE_TEST_RUN_EXIT_CODE%

