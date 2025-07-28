#!/bin/bash

# =============================================================================
# OpenASP System Boot Script
# 모든 프로젝트 서비스를 시작하는 통합 스크립트
# =============================================================================

set -e  # 에러 발생시 스크립트 중단

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_FILE="$SCRIPT_DIR/logs/system_boot.log"
PID_DIR="$SCRIPT_DIR/pids"

# 로그 디렉토리 생성
mkdir -p "$SCRIPT_DIR/logs"
mkdir -p "$PID_DIR"

# 로그 함수
log() {
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "[$timestamp] $1" | tee -a "$LOG_FILE"
}

# PID 저장 함수
save_pid() {
    local service_name="$1"
    local pid="$2"
    echo "$pid" > "$PID_DIR/${service_name}.pid"
}

# 서비스 시작 확인 함수
wait_for_service() {
    local port="$1"
    local service_name="$2"
    local max_wait=30
    local count=0
    
    log "포트 $port에서 $service_name 서비스 시작 대기중..."
    
    while ! nc -z localhost "$port" && [ $count -lt $max_wait ]; do
        sleep 1
        count=$((count + 1))
    done
    
    if [ $count -eq $max_wait ]; then
        log "ERROR: $service_name 서비스가 $max_wait초 내에 시작되지 않았습니다."
        return 1
    else
        log "SUCCESS: $service_name 서비스가 포트 $port에서 시작되었습니다."
        return 0
    fi
}

log "=========================================="
log "OpenASP System Boot 시작"
log "=========================================="

# 기존 실행중인 서비스 확인
log "기존 실행중인 서비스 확인..."
existing_services=$(ps aux | grep -E "(react-scripts|api_server|python.*conversion)" | grep -v grep | wc -l)
if [ $existing_services -gt 0 ]; then
    log "WARNING: 이미 실행중인 서비스가 있습니다. down.sh를 먼저 실행하는 것을 권장합니다."
    ps aux | grep -E "(react-scripts|api_server|python.*conversion)" | grep -v grep | head -5
fi

# 1. ASP Manager (포트 3000)
log "1. ASP Manager 시작 중..."
cd "$SCRIPT_DIR/asp-manager"
if [ -f "package.json" ]; then
    PORT=3000 nohup npm start > "$SCRIPT_DIR/logs/asp-manager.log" 2>&1 &
    ASP_MANAGER_PID=$!
    save_pid "asp-manager" "$ASP_MANAGER_PID"
    log "ASP Manager 시작됨 (PID: $ASP_MANAGER_PID)"
else
    log "WARNING: asp-manager package.json을 찾을 수 없습니다."
fi

# 2. OpenASP Refactor (포트 3005)
log "2. OpenASP Refactor 시작 중..."
cd "$SCRIPT_DIR/ofasp-refactor"
if [ -f "package.json" ]; then
    PORT=3005 nohup npm start > "$SCRIPT_DIR/logs/ofasp-refactor.log" 2>&1 &
    OFASP_REFACTOR_PID=$!
    save_pid "ofasp-refactor" "$OFASP_REFACTOR_PID"
    log "OpenASP Refactor 시작됨 (PID: $OFASP_REFACTOR_PID)"
else
    log "WARNING: ofasp-refactor package.json을 찾을 수 없습니다."
fi

# 3. Main React App (포트 3007)
log "3. Main React App 시작 중..."
cd "$SCRIPT_DIR"
if [ -f "package.json" ]; then
    PORT=3007 nohup npm start > "$SCRIPT_DIR/logs/main-app.log" 2>&1 &
    MAIN_APP_PID=$!
    save_pid "main-app" "$MAIN_APP_PID"
    log "Main React App 시작됨 (PID: $MAIN_APP_PID)"
else
    log "WARNING: main app package.json을 찾을 수 없습니다."
fi

# 4. API Server (포트 5000)
log "4. API Server 시작 중..."
cd "$SCRIPT_DIR/server"
if [ -f "api_server.py" ]; then
    nohup python api_server.py > "$SCRIPT_DIR/logs/api-server.log" 2>&1 &
    API_SERVER_PID=$!
    save_pid "api-server" "$API_SERVER_PID"
    log "API Server 시작됨 (PID: $API_SERVER_PID)"
else
    log "WARNING: api_server.py를 찾을 수 없습니다."
fi

# 5. EBCDIC Conversion Service (포트 8080)
log "5. EBCDIC Conversion Service 시작 중..."
cd "$SCRIPT_DIR/ofasp-refactor/python-service"
if [ -f "main.py" ]; then
    nohup python main.py > "$SCRIPT_DIR/logs/ebcdic-service.log" 2>&1 &
    EBCDIC_SERVICE_PID=$!
    save_pid "ebcdic-service" "$EBCDIC_SERVICE_PID"
    log "EBCDIC Conversion Service 시작됨 (PID: $EBCDIC_SERVICE_PID)"
else
    log "WARNING: EBCDIC conversion service main.py를 찾을 수 없습니다."
fi

# 서비스 시작 대기
log "서비스들의 시작을 대기 중..."
sleep 5

# 서비스 상태 확인
log "=========================================="
log "서비스 상태 확인"
log "=========================================="

# 포트 확인
ports=("3000:ASP-Manager" "3005:OFASP-Refactor" "3007:Main-App" "8000:API-Server" "8080:EBCDIC-Service")

for port_service in "${ports[@]}"; do
    port="${port_service%:*}"
    service="${port_service#*:}"
    
    if nc -z localhost "$port" 2>/dev/null; then
        log "✓ $service (포트 $port): 실행 중"
    else
        log "✗ $service (포트 $port): 연결 실패"
    fi
done

# 프로세스 상태 확인
log ""
log "실행 중인 프로세스:"
ps aux | grep -E "(react-scripts|api_server|python.*main)" | grep -v grep | while read line; do
    log "  $line"
done

# 완료 메시지
log ""
log "=========================================="
log "OpenASP System Boot 완료"
log "=========================================="
log "웹 인터페이스 접속 주소:"
log "  - ASP Manager:     http://localhost:3000"
log "  - OFASP Refactor:  http://localhost:3005"
log "  - Main App:        http://localhost:3007"
log "  - API Server:      http://localhost:8000"
log "  - EBCDIC Service:  http://localhost:8080"
log ""
log "시스템 정지: ./down.sh"
log "로그 확인: tail -f $LOG_FILE"
log "=========================================="

cd "$SCRIPT_DIR"