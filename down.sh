#!/bin/bash

# =============================================================================
# OpenASP System Shutdown Script
# 모든 프로젝트 서비스를 정지하는 통합 스크립트
# =============================================================================

set -e  # 에러 발생시 스크립트 중단

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_FILE="$SCRIPT_DIR/logs/system_shutdown.log"
PID_DIR="$SCRIPT_DIR/pids"

# 로그 디렉토리 생성
mkdir -p "$SCRIPT_DIR/logs"

# 로그 함수
log() {
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "[$timestamp] $1" | tee -a "$LOG_FILE"
}

# PID로 프로세스 종료 함수
kill_by_pid() {
    local service_name="$1"
    local pid_file="$PID_DIR/${service_name}.pid"
    
    if [ -f "$pid_file" ]; then
        local pid=$(cat "$pid_file")
        if kill -0 "$pid" 2>/dev/null; then
            log "PID $pid로 $service_name 종료 중..."
            kill "$pid" 2>/dev/null || true
            sleep 2
            
            # 강제 종료가 필요한 경우
            if kill -0 "$pid" 2>/dev/null; then
                log "강제 종료: $service_name (PID: $pid)"
                kill -9 "$pid" 2>/dev/null || true
            fi
        else
            log "$service_name PID $pid가 이미 종료되었습니다."
        fi
        rm -f "$pid_file"
    else
        log "$service_name PID 파일을 찾을 수 없습니다: $pid_file"
    fi
}

# 패턴으로 프로세스 종료 함수
kill_by_pattern() {
    local pattern="$1"
    local service_name="$2"
    
    log "$service_name 프로세스 검색 중... (패턴: $pattern)"
    
    local pids=$(ps aux | grep -E "$pattern" | grep -v grep | awk '{print $2}')
    
    if [ -n "$pids" ]; then
        log "$service_name 프로세스 발견, 종료 중..."
        echo "$pids" | while read pid; do
            if [ -n "$pid" ]; then
                log "  프로세스 종료: PID $pid"
                kill "$pid" 2>/dev/null || true
            fi
        done
        
        sleep 3
        
        # 강제 종료 확인
        local remaining_pids=$(ps aux | grep -E "$pattern" | grep -v grep | awk '{print $2}')
        if [ -n "$remaining_pids" ]; then
            log "$service_name 강제 종료 중..."
            echo "$remaining_pids" | while read pid; do
                if [ -n "$pid" ]; then
                    kill -9 "$pid" 2>/dev/null || true
                fi
            done
        fi
    else
        log "$service_name 실행 중인 프로세스가 없습니다."
    fi
}

log "=========================================="
log "OpenASP System Shutdown 시작"
log "=========================================="

# 현재 실행 중인 서비스 확인
log "실행 중인 서비스 확인..."
ps aux | grep -E "(react-scripts|api_server|python.*main)" | grep -v grep | while read line; do
    log "  $line"
done

# 1. PID 파일 기반 서비스 종료
log ""
log "1. PID 파일 기반 서비스 종료..."
services=("asp-manager" "ofasp-refactor" "main-app" "api-server" "ebcdic-service")

for service in "${services[@]}"; do
    kill_by_pid "$service"
done

# 2. 패턴 기반 서비스 종료
log ""
log "2. 패턴 기반 서비스 종료..."

# React 개발 서버들
kill_by_pattern "react-scripts.*start" "React Development Servers"

# Node.js 프로세스들 (React scripts 관련)
kill_by_pattern "node.*react-scripts" "Node.js React Scripts"

# TypeScript checker 프로세스들
kill_by_pattern "TypeScriptReporterRpcService" "TypeScript Checker Services"

# API Server
kill_by_pattern "python.*api_server.py" "API Server"

# EBCDIC Conversion Service
kill_by_pattern "python.*main.py.*conversion" "EBCDIC Conversion Service"

# 기타 Python 서비스들
kill_by_pattern "python.*src.api.app" "Python API Services"

# 3. 포트 기반 프로세스 확인 및 종료
log ""
log "3. 포트 기반 프로세스 확인..."
ports=(3000 3003 3004 3005 3007 3008 8000 8080)

for port in "${ports[@]}"; do
    pid=$(lsof -ti:$port 2>/dev/null || true)
    if [ -n "$pid" ]; then
        log "포트 $port 사용 중인 프로세스 종료: PID $pid"
        kill "$pid" 2>/dev/null || true
        sleep 1
        
        # 강제 종료 확인
        remaining_pid=$(lsof -ti:$port 2>/dev/null || true)
        if [ -n "$remaining_pid" ]; then
            log "포트 $port 강제 종료: PID $remaining_pid"
            kill -9 "$remaining_pid" 2>/dev/null || true
        fi
    else
        log "포트 $port: 사용 중인 프로세스 없음"
    fi
done

# 4. 정리 작업
log ""
log "4. 정리 작업..."

# PID 디렉토리 정리
if [ -d "$PID_DIR" ]; then
    rm -rf "$PID_DIR"
    log "PID 디렉토리 정리 완료"
fi

# 임시 파일 정리
find "$SCRIPT_DIR" -name "*.pid" -delete 2>/dev/null || true
find "$SCRIPT_DIR" -name "nohup.out" -delete 2>/dev/null || true

# 5. 최종 상태 확인
log ""
log "5. 최종 상태 확인..."
sleep 2

remaining_processes=$(ps aux | grep -E "(react-scripts|api_server|python.*main)" | grep -v grep | wc -l)

if [ $remaining_processes -eq 0 ]; then
    log "✓ 모든 서비스가 성공적으로 종료되었습니다."
else
    log "⚠ 일부 프로세스가 여전히 실행 중입니다:"
    ps aux | grep -E "(react-scripts|api_server|python.*main)" | grep -v grep | while read line; do
        log "  $line"
    done
fi

# 포트 상태 확인
log ""
log "포트 상태 확인:"
for port in 3000 3003 3004 3005 3007 3008 8000 8080; do
    if nc -z localhost "$port" 2>/dev/null; then
        log "⚠ 포트 $port: 여전히 사용 중"
    else
        log "✓ 포트 $port: 해제됨"
    fi
done

log ""
log "=========================================="
log "OpenASP System Shutdown 완료"
log "=========================================="
log "시스템 재시작: ./boot.sh"
log "로그 확인: tail -f $LOG_FILE"
log "=========================================="