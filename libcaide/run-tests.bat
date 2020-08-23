@echo off
"%MSYS_DIR%"\msys2_shell.cmd -full-path -here -no-start -defterm -c "tests/run-tests.sh %*"

