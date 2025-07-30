@echo off
setlocal enabledelayedexpansion

REM ================================================
REM Docker Live Backup Script (English Version)
REM Target: react-dev-docker-react-dev container
REM ================================================

REM Set current date (YYYYMMDD format)
for /f "tokens=2 delims==" %%a in ('wmic OS Get localdatetime /value') do set "dt=%%a"
set "YY=%dt:~2,2%" & set "YYYY=%dt:~0,4%" & set "MM=%dt:~4,2%" & set "DD=%dt:~6,2%"
set "HH=%dt:~8,2%" & set "MIN=%dt:~10,2%" & set "SS=%dt:~12,2%"
set "datestamp=%YYYY%%MM%%DD%"
set "timestamp=%YYYY%%MM%%DD%_%HH%%MIN%%SS%"

REM Set backup directory
set "BACKUP_DIR=docker_backup_%datestamp%"
set "CONTAINER_NAME=react-dev-docker-react-dev"

echo ================================================
echo Docker Live Backup Started
echo Time: %date% %time%
echo Container: %CONTAINER_NAME%
echo Backup Directory: %BACKUP_DIR%
echo ================================================

REM Create backup directory
if not exist "%BACKUP_DIR%" (
    mkdir "%BACKUP_DIR%"
    echo Backup directory created: %BACKUP_DIR%
) else (
    echo Backup directory already exists: %BACKUP_DIR%
)

REM Check if Docker is running
echo Checking Docker service status...
docker version >nul 2>&1
if errorlevel 1 (
    echo ERROR: Docker is not running.
    echo Please start Docker Desktop and try again.
    pause
    exit /b 1
)

REM Check if container is running
echo Checking container %CONTAINER_NAME% status...
docker ps --filter "name=%CONTAINER_NAME%" --format "{{.Names}}" | findstr /C:"%CONTAINER_NAME%" >nul
if errorlevel 1 (
    echo WARNING: Container %CONTAINER_NAME% is not running.
    echo Running containers list:
    docker ps --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}"
    echo.
    echo Do you want to continue? (y/n)
    set /p choice=
    if /i not "%choice%"=="y" exit /b 1
)

REM 1. Commit running container to image
echo.
echo [1/4] Committing container to image...
set "BACKUP_IMAGE=%CONTAINER_NAME%-backup-%timestamp%"
docker commit %CONTAINER_NAME% %BACKUP_IMAGE%
if errorlevel 1 (
    echo ERROR: Container commit failed.
    pause
    exit /b 1
)
echo Commit completed: %BACKUP_IMAGE%

REM 2. Save committed image to tar file
echo.
echo [2/4] Saving image to tar file...
set "TAR_FILE=%BACKUP_DIR%\%CONTAINER_NAME%-image-%timestamp%.tar"
docker save -o "%TAR_FILE%" %BACKUP_IMAGE%
if errorlevel 1 (
    echo ERROR: Image save failed.
    pause
    exit /b 1
)
echo Image saved: %TAR_FILE%

REM 3. Backup container volumes
echo.
echo [3/4] Backing up container volumes...
set "VOLUME_BACKUP=%BACKUP_DIR%\%CONTAINER_NAME%-volumes-%timestamp%.tar"
docker run --rm --volumes-from %CONTAINER_NAME% -v "%cd%\%BACKUP_DIR%":/backup alpine tar czf /backup/%CONTAINER_NAME%-volumes-%timestamp%.tar.gz -C / --exclude=/proc --exclude=/sys --exclude=/dev --exclude=/backup .
if errorlevel 1 (
    echo WARNING: Volume backup failed. Container may have no volumes.
) else (
    echo Volume backup completed: %BACKUP_DIR%\%CONTAINER_NAME%-volumes-%timestamp%.tar.gz
)

REM 4. Backup container configuration
echo.
echo [4/4] Backing up container configuration...
docker inspect %CONTAINER_NAME% > "%BACKUP_DIR%\%CONTAINER_NAME%-config-%timestamp%.json"
docker ps --filter "name=%CONTAINER_NAME%" --format "table {{.Names}}\t{{.Image}}\t{{.Ports}}\t{{.Status}}\t{{.CreatedAt}}" > "%BACKUP_DIR%\%CONTAINER_NAME%-status-%timestamp%.txt"
echo Configuration backup completed

REM 5. Cleanup after backup
echo.
echo [Cleanup] Removing temporary backup image...
docker rmi %BACKUP_IMAGE% >nul 2>&1
if errorlevel 1 (
    echo WARNING: Failed to remove temporary image. Please remove manually: %BACKUP_IMAGE%
) else (
    echo Temporary image removed
)

REM Backup summary
echo.
echo ================================================
echo Backup Completed Successfully!
echo ================================================
echo Backup time: %date% %time%
echo Backup location: %cd%\%BACKUP_DIR%
echo.
echo Backed up files:
dir "%BACKUP_DIR%" /b
echo.

REM Check backup file sizes
echo Backup file sizes:
for %%F in ("%BACKUP_DIR%\*.*") do (
    set "size=%%~zF"
    set /a "sizeMB=!size!/1024/1024"
    echo   %%~nxF: !sizeMB! MB
)

echo.
echo Restore instructions:
echo   1. Restore image: docker load -i "%TAR_FILE%"
echo   2. Run container: docker run -d --name new-container %BACKUP_IMAGE%
echo.

REM Create log file
echo %date% %time% - Backup completed: %BACKUP_DIR% >> docker_backup.log

echo Backup completed successfully.
pause