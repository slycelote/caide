rem Courtesy of http://blogs.clariusconsulting.net/kzu/resetting-visual-studio-experimental-instance-to-its-super-clean-initial-state/
rem Should be called with all instances of VS Experimental closed

del /S /Q %APPDATA%\Microsoft\VisualStudio\12.0Exp
reg delete HKEY_CURRENT_USER\Software\Microsoft\VisualStudio\12.0Exp /f
reg delete HKEY_CURRENT_USER\Software\Microsoft\VisualStudio\12.0Exp_Config /f

C:\windows\system32\Cmd.exe /C "C:\Program Files (x86)\Microsoft Visual Studio 12.0\VSSDK\VisualStudioIntegration\Tools\Bin\CreateExpInstance.exe" /Reset /VSInstance=12.0 /RootSuffix=Exp && PAUSE
