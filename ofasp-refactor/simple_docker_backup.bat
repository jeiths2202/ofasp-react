@echo off
REM ================================================
REM 간단한 Docker 백업 스크립트
REM ================================================

REM 날짜 설정
for /f "tokens=2 delims==" %%a in ('wmic OS Get localdatetime /value') do set "dt=%%a"
set "datestamp=%dt:~0,4%%dt:~4,2%%dt:~6,2%"
set "timestamp=%dt:~0,4%%dt:~4,2%%dt:~6,2%_%dt:~8,2%%dt:~10,2%%dt:~12,2%"

REM 설정
set "CONTAINER_NAME=react-dev-docker-react-dev"
set "BACKUP_DIR=docker_backup_%datestamp%"

echo Docker 백업 시작: %CONTAINER_NAME%
echo 백업 디렉토리: %BACKUP_DIR%

REM 백업 디렉토리 생성
mkdir "%BACKUP_DIR%" 2>nul

REM 컨테이너를 이미지로 커밋
echo 컨테이너 커밋 중...
docker commit %CONTAINER_NAME% %CONTAINER_NAME%-backup:%timestamp%

REM 이미지를 tar 파일로 저장
echo 이미지 저장 중...
docker save -o "%BACKUP_DIR%\%CONTAINER_NAME%-backup-%timestamp%.tar" %CONTAINER_NAME%-backup:%timestamp%

REM 컨테이너 설정 백업
echo 설정 백업 중...
docker inspect %CONTAINER_NAME% > "%BACKUP_DIR%\container-config-%timestamp%.json"

REM 임시 이미지 삭제
docker rmi %CONTAINER_NAME%-backup:%timestamp% >nul 2>&1

echo.
echo 백업 완료! 위치: %BACKUP_DIR%
echo 백업된 파일:
dir "%BACKUP_DIR%" /b

pause