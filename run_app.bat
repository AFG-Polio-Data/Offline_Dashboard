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

rem === Optional: logging ===
set "LOG_FILE=%BASE_DIR%run_log.txt"
if exist "%LOG_FILE%" del "%LOG_FILE%"
echo Launching Shiny app... > "%LOG_FILE%"
echo R_EXE: %R_EXE% >> "%LOG_FILE%"
echo APP_DIR_R: %APP_DIR_R% >> "%LOG_FILE%"
echo. >> "%LOG_FILE%"

rem === Run Shiny app ===
"%R_EXE%" -e "options(shiny.launch.browser=TRUE); shiny::runApp('%APP_DIR_R%', host='127.0.0.1', port=4242)" >> "%LOG_FILE%" 2>&1

if %errorlevel% neq 0 (
    echo. >> "%LOG_FILE%"
    echo Rscript exited with code %errorlevel%. >> "%LOG_FILE%"
)
exit
