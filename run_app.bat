@echo off
cd /d "%~dp0"

rem === Locate Rscript inside portable R ===
set "R_EXE=%~dp0R-Portable\App\R-Portable\bin\x64\Rscript.exe"

rem === Ensure trailing backslash and set app dir ===
set "BASE_DIR=%~dp0"
if not "%BASE_DIR:~-1%"=="\" set "BASE_DIR=%BASE_DIR%\" 
set "APP_DIR=%BASE_DIR%revised_app"

rem === Convert backslashes to forward slashes for R ===
set "APP_DIR_R=%APP_DIR:\=/%"

rem === Logging inside revised_app ===
set "LOGFILE=%~dp0revised_app\run_log.txt"
del "%LOGFILE%" 2>nul
echo Launching Shiny app... > "%LOGFILE%"
echo R_EXE: %R_EXE% >> "%LOGFILE%"
echo APP_DIR_R: %APP_DIR_R% >> "%LOGFILE%"
echo. >> "%LOGFILE%"

rem === Run Shiny app ===
"%R_EXE%" -e "options(shiny.launch.browser=TRUE); shiny::runApp('%APP_DIR_R%', host='127.0.0.1', port=4242)" >> "%LOGFILE%" 2>&1

if %errorlevel% neq 0 (
    echo. >> "%LOGFILE%"
    echo Rscript exited with code %errorlevel%. >> "%LOGFILE%"
)
exit
