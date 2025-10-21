@echo off
cd /d "%~dp0"
echo Launching APMIS data update...

set "R_EXE=%~dp0R-Portable\App\R-Portable\bin\x64\Rscript.exe"
"%R_EXE%" "%~dp0update_data.R"

echo.
echo Update finished. Press any key to close this window.
pause >nul
