@echo off
setlocal enabledelayedexpansion

REM ================================================
REM Docker 무중단 백업 스크립트
REM 대상: react-dev-docker-react-dev 컨테이너
REM ================================================

REM 현재 날짜 설정 (YYYYMMDD 형식)
for /f "tokens=2 delims==" %%a in ('wmic OS Get localdatetime /value') do set "dt=%%a"
set "YY=%dt:~2,2%" & set "YYYY=%dt:~0,4%" & set "MM=%dt:~4,2%" & set "DD=%dt:~6,2%"
set "HH=%dt:~8,2%" & set "MIN=%dt:~10,2%" & set "SS=%dt:~12,2%"
set "datestamp=%YYYY%%MM%%DD%"
set "timestamp=%YYYY%%MM%%DD%_%HH%%MIN%%SS%"

REM 백업 디렉토리 설정
set "BACKUP_DIR=docker_backup_%datestamp%"
set "CONTAINER_NAME=react-dev-docker-react-dev"

echo ================================================
echo Docker 무중단 백업 시작
echo 시간: %date% %time%
echo 컨테이너: %CONTAINER_NAME%
echo 백업 디렉토리: %BACKUP_DIR%
echo ================================================

REM 백업 디렉토리 생성
if not exist "%BACKUP_DIR%" (
    mkdir "%BACKUP_DIR%"
    echo 백업 디렉토리 생성: %BACKUP_DIR%
) else (
    echo 백업 디렉토리 이미 존재: %BACKUP_DIR%
)

REM Docker가 실행 중인지 확인
echo Docker 서비스 상태 확인 중...
docker version >nul 2>&1
if errorlevel 1 (
    echo 오류: Docker가 실행되지 않고 있습니다.
    echo Docker Desktop을 시작하고 다시 시도해주세요.
    pause
    exit /b 1
)

REM 컨테이너가 실행 중인지 확인
echo 컨테이너 %CONTAINER_NAME% 상태 확인 중...
docker ps --filter "name=%CONTAINER_NAME%" --format "{{.Names}}" | findstr /C:"%CONTAINER_NAME%" >nul
if errorlevel 1 (
    echo 경고: 컨테이너 %CONTAINER_NAME%가 실행되지 않고 있습니다.
    echo 실행 중인 컨테이너 목록:
    docker ps --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}"
    echo.
    echo 계속하시겠습니까? (y/n)
    set /p choice=
    if /i not "%choice%"=="y" exit /b 1
)

REM 1. 실행 중인 컨테이너를 이미지로 커밋
echo.
echo [1/4] 컨테이너를 이미지로 커밋 중...
set "BACKUP_IMAGE=%CONTAINER_NAME%-backup-%timestamp%"
docker commit %CONTAINER_NAME% %BACKUP_IMAGE%
if errorlevel 1 (
    echo 오류: 컨테이너 커밋에 실패했습니다.
    pause
    exit /b 1
)
echo 커밋 완료: %BACKUP_IMAGE%

REM 2. 커밋된 이미지를 tar 파일로 저장
echo.
echo [2/4] 이미지를 tar 파일로 저장 중...
set "TAR_FILE=%BACKUP_DIR%\%CONTAINER_NAME%-image-%timestamp%.tar"
docker save -o "%TAR_FILE%" %BACKUP_IMAGE%
if errorlevel 1 (
    echo 오류: 이미지 저장에 실패했습니다.
    pause
    exit /b 1
)
echo 이미지 저장 완료: %TAR_FILE%

REM 3. 컨테이너 볼륨 백업
echo.
echo [3/4] 컨테이너 볼륨 백업 중...
set "VOLUME_BACKUP=%BACKUP_DIR%\%CONTAINER_NAME%-volumes-%timestamp%.tar"
docker run --rm --volumes-from %CONTAINER_NAME% -v "%cd%\%BACKUP_DIR%":/backup alpine tar czf /backup/%CONTAINER_NAME%-volumes-%timestamp%.tar.gz -C / --exclude=/proc --exclude=/sys --exclude=/dev --exclude=/backup .
if errorlevel 1 (
    echo 경고: 볼륨 백업에 실패했습니다. 컨테이너에 볼륨이 없을 수 있습니다.
) else (
    echo 볼륨 백업 완료: %BACKUP_DIR%\%CONTAINER_NAME%-volumes-%timestamp%.tar.gz
)

REM 4. 컨테이너 설정 정보 백업
echo.
echo [4/4] 컨테이너 설정 정보 백업 중...
docker inspect %CONTAINER_NAME% > "%BACKUP_DIR%\%CONTAINER_NAME%-config-%timestamp%.json"
docker ps --filter "name=%CONTAINER_NAME%" --format "table {{.Names}}\t{{.Image}}\t{{.Ports}}\t{{.Status}}\t{{.CreatedAt}}" > "%BACKUP_DIR%\%CONTAINER_NAME%-status-%timestamp%.txt"
echo 설정 정보 백업 완료

REM 5. 백업 완료 후 정리
echo.
echo [정리] 임시 백업 이미지 삭제 중...
docker rmi %BACKUP_IMAGE% >nul 2>&1
if errorlevel 1 (
    echo 경고: 임시 이미지 삭제에 실패했습니다. 수동으로 삭제해주세요: %BACKUP_IMAGE%
) else (
    echo 임시 이미지 삭제 완료
)

REM 백업 결과 요약
echo.
echo ================================================
echo 백업 완료!
echo ================================================
echo 백업 시간: %date% %time%
echo 백업 위치: %cd%\%BACKUP_DIR%
echo.
echo 백업된 파일들:
dir "%BACKUP_DIR%" /b
echo.

REM 백업 파일 크기 확인
echo 백업 파일 크기:
for %%F in ("%BACKUP_DIR%\*.*") do (
    set "size=%%~zF"
    set /a "sizeMB=!size!/1024/1024"
    echo   %%~nxF: !sizeMB! MB
)

echo.
echo 복원 방법:
echo   1. 이미지 복원: docker load -i "%TAR_FILE%"
echo   2. 컨테이너 실행: docker run -d --name new-container %BACKUP_IMAGE%
echo.

REM 로그 파일 생성
echo %date% %time% - 백업 완료: %BACKUP_DIR% >> docker_backup.log

echo 백업이 성공적으로 완료되었습니다.
pause