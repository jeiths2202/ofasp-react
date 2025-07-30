#!/bin/bash
# OpenASP AX Complete Development Environment Master Startup Script (Based on PROJECT_CONTEXT.json)

echo "[START] OpenASP AX Complete Development Environment Startup..."
echo "========================================="

# Color definitions
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Project root directory
APP_ROOT="/home/aspuser/app"

# Create log, pid and database directories
mkdir -p "$APP_ROOT/logs"
mkdir -p "$APP_ROOT/pids"
mkdir -p "$APP_ROOT/database"

# Cleanup existing services function
cleanup_existing() {
    echo -e "${YELLOW}[LIST] Cleaning up existing processes...${NC}"
    
    # Try graceful termination first
    pkill -f "react-scripts" 2>/dev/null || true
    pkill -f "flask.*3003" 2>/dev/null || true
    pkill -f "python.*api_server" 2>/dev/null || true
    
    sleep 3
    
    # Check ports and force kill if necessary
    for port in 3000 3003 3005 3007 8000; do
        local pids=$(lsof -ti:$port 2>/dev/null || true)
        if [ ! -z "$pids" ]; then
            echo "  Force killing processes using port $port..."
            echo "$pids" | xargs -r kill -9 2>/dev/null || true
        fi
    done
    
    sleep 2
}

# Service startup function
start_service() {
    local name="$1"
    local port="$2"
    local command="$3"
    local log_file="$4"
    local pid_file="$5"
    
    echo -e "\n${GREEN}$name startup (Port $port)...${NC}"
    
    # Initialize log file
    > "$log_file"
    
    # Start service
    eval "$command" > "$log_file" 2>&1 &
    local pid=$!
    
    # Save PID
    echo "$pid" > "$pid_file"
    echo "$name PID: $pid"
    
    return $pid
}

# Service status check function
check_service() {
    local name="$1"
    local port="$2"
    local log_file="$3"
    local max_wait=60
    local count=0
    
    while [ $count -lt $max_wait ]; do
        if curl -s http://localhost:$port >/dev/null 2>&1; then
            echo -e "${GREEN}[OK] $name${NC} - http://localhost:$port"
            return 0
        elif [ $port -eq 8000 ]; then
            # API Server accepts 404 as normal response
            local status=$(curl -s -o /dev/null -w "%{http_code}" http://localhost:$port 2>/dev/null || echo "000")
            if echo "$status" | grep -E "200|404" >/dev/null; then
                echo -e "${GREEN}[OK] $name${NC} - http://localhost:$port"
                return 0
            fi
        fi
        
        sleep 2
        count=$((count + 2))
        
        if [ $((count % 10)) -eq 0 ]; then
            echo "  $name waiting... ($count/${max_wait}sec)"
        fi
    done
    
    echo -e "${RED}[NG] $name startup failed${NC}"
    echo "   Log check: tail -f $log_file"
    return 1
}

# Cleanup existing processes
cleanup_existing

# Initialize OpenASP Job Database
echo -e "\n${YELLOW}[DB] Initializing OpenASP Job Database...${NC}"
cd "$APP_ROOT/server/system-cmds"
python -c "from functions.job_database import init_database; init_database()" 2>/dev/null || {
    echo -e "${RED}[NG] Job database initialization failed${NC}"
}
echo -e "${GREEN}[OK] Job database ready${NC}"

# 1. Python EBCDIC Conversion Service startup (Port 3003)
cd "$APP_ROOT/ofasp-refactor/python-service"
if [ -f "src/api/app.py" ]; then
    start_service "[PYTHON] Python Conversion Service" 3003 \
        "FLASK_PORT=3003 python -c \"from src.api.app import api; api.run()\"" \
        "$APP_ROOT/logs/python-service.log" \
        "$APP_ROOT/pids/python-service.pid"
    PYTHON_PID=$!
else
    echo -e "${RED}[NG] Python service app.py not found.${NC}"
fi

# 2. SMED Map Viewer startup (Port 3000)
cd "$APP_ROOT"
if [ -f "package.json" ]; then
    start_service "[MAP] SMED Map Viewer" 3000 \
        "npm start" \
        "$APP_ROOT/logs/smed-viewer.log" \
        "$APP_ROOT/pids/smed-viewer.pid"
    SMED_PID=$!
else
    echo -e "${RED}[NG] Main package.json not found.${NC}"
fi

# 3. OpenASP Refactor startup (Port 3005)
cd "$APP_ROOT/ofasp-refactor"
if [ -f "package.json" ]; then
    start_service "[REACT] OpenASP Refactor" 3005 \
        "PORT=3005 npm start" \
        "$APP_ROOT/logs/ofasp-refactor.log" \
        "$APP_ROOT/pids/ofasp-refactor.pid"
    REFACTOR_PID=$!
else
    echo -e "${RED}[NG] ofasp-refactor package.json not found.${NC}"
fi

# 4. ASP Manager startup (Port 3007)
cd "$APP_ROOT/asp-manager"
if [ -f "package.json" ]; then
    start_service "[TARGET] ASP Manager" 3007 \
        "PORT=3007 npm start" \
        "$APP_ROOT/logs/asp-manager.log" \
        "$APP_ROOT/pids/asp-manager.pid"
    MANAGER_PID=$!
else
    echo -e "${RED}[NG] asp-manager package.json not found.${NC}"
fi

# 5. API Server startup (Port 8000)
cd "$APP_ROOT/server"
if [ -f "api_server.py" ]; then
    start_service "[TOOL] API Server" 8000 \
        "python api_server.py" \
        "$APP_ROOT/logs/api-server.log" \
        "$APP_ROOT/pids/api-server.pid"
    API_SERVER_PID=$!
else
    echo -e "${RED}[NG] api_server.py not found.${NC}"
fi

# 6. System API Server startup (Port 3004)
cd "$APP_ROOT/ofasp-refactor/server"
if [ -f "aspmgr_web.py" ]; then
    start_service "[SYSTEM] System API Server" 3004 \
        "ASPMGR_WEB_PORT=3004 python aspmgr_web.py" \
        "$APP_ROOT/logs/system-api.log" \
        "$APP_ROOT/pids/system-api.pid"
    SYSTEM_API_PID=$!
else
    echo -e "${RED}[NG] aspmgr_web.py not found.${NC}"
fi

# Wait for service startup
echo -e "\n${YELLOW}[WAIT] Waiting for service startup...${NC}"
sleep 10

# Service status check
echo -e "\n${YELLOW}[CHECK] Service status check...${NC}"
echo "========================================="

# Check each service status
check_service "Python Conversion Service" 3003 "$APP_ROOT/logs/python-service.log"
check_service "SMED Map Viewer" 3000 "$APP_ROOT/logs/smed-viewer.log"
check_service "OpenASP Refactor" 3005 "$APP_ROOT/logs/ofasp-refactor.log"
check_service "ASP Manager" 3007 "$APP_ROOT/logs/asp-manager.log"
check_service "API Server" 8000 "$APP_ROOT/logs/api-server.log"
check_service "System API Server" 3004 "$APP_ROOT/logs/system-api.log"

# Save process information
echo -e "\n${YELLOW}[SAVE] Saving process information...${NC}"
cat > "$APP_ROOT/.running_services" << EOF
PYTHON_SERVICE_PID=$PYTHON_PID
SMED_VIEWER_PID=$SMED_PID
REFACTOR_APP_PID=$REFACTOR_PID
MANAGER_APP_PID=$MANAGER_PID
API_SERVER_PID=$API_SERVER_PID
SYSTEM_API_PID=$SYSTEM_API_PID
STARTED_AT="$(date)"
EOF

echo "========================================="
echo -e "${GREEN}[DONE] OpenASP AX Development Environment startup complete!${NC}"
echo ""
echo "[MOBILE] Main service connection URLs:"
echo "   - SMED Map Viewer: http://localhost:3000"
echo "   - Python Conversion Service: http://localhost:3003"
echo "   - System API Server: http://localhost:3004"
echo "   - OpenASP Refactor: http://localhost:3005"
echo "   - ASP Manager: http://localhost:3007"
echo "   - API Server: http://localhost:8000"
echo ""
echo "[LIST] Log files:"
echo "   - Python Service: $APP_ROOT/logs/python-service.log"
echo "   - SMED Viewer: $APP_ROOT/logs/smed-viewer.log"
echo "   - Refactor: $APP_ROOT/logs/ofasp-refactor.log"
echo "   - Manager: $APP_ROOT/logs/asp-manager.log"
echo "   - System API: $APP_ROOT/logs/system-api.log"
echo "   - API Server: $APP_ROOT/logs/api-server.log"
echo ""
echo "[STOP] Complete shutdown command:"
echo "   $APP_ROOT/master-stop.sh"
echo ""

cd "$APP_ROOT"
