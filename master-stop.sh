#!/bin/bash
# OpenASP AX Complete Development Environment Shutdown Script

echo "[STOP] OpenASP AX Complete Development Environment Shutdown..."
echo "========================================="

# Color definitions
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Force kill processes by pattern
force_kill_by_pattern() {
    local pattern="$1"
    local description="$2"
    
    echo -n "$description shutdown... "
    
    # Try graceful termination with SIGTERM first
    pkill -f "$pattern" 2>/dev/null || true
    sleep 2
    
    # Force kill with SIGKILL if still running
    pkill -9 -f "$pattern" 2>/dev/null || true
    
    echo -e "${GREEN}[OK]${NC}"
}

# Force kill processes by port
force_kill_by_port() {
    local port="$1"
    local description="$2"
    
    echo -n "$description (Port $port) shutdown... "
    
    # Find processes using the port
    local pids=$(lsof -ti:$port 2>/dev/null || true)
    
    if [ ! -z "$pids" ]; then
        # Try SIGTERM first
        echo "$pids" | xargs -r kill 2>/dev/null || true
        sleep 2
        
        # Force kill if still running
        local remaining_pids=$(lsof -ti:$port 2>/dev/null || true)
        if [ ! -z "$remaining_pids" ]; then
            echo "$remaining_pids" | xargs -r kill -9 2>/dev/null || true
        fi
    fi
    
    echo -e "${GREEN}[OK]${NC}"
}

echo -e "\n${YELLOW}Shutting down processes by pattern...${NC}"

# React development servers
force_kill_by_pattern "react-scripts" "React Development Servers"

# Node.js related processes
force_kill_by_pattern "webpack-dev-server" "Webpack Dev Servers"
force_kill_by_pattern "fork-ts-checker" "TypeScript Checkers"

# Python services
force_kill_by_pattern "flask.*3003" "Python Flask Services"
force_kill_by_pattern "python.*api.run" "Python API Services"
force_kill_by_pattern "python.*api_server" "Python API Servers"
force_kill_by_pattern "python.*aspmgr_web" "System API Servers"

echo -e "\n${YELLOW}Force killing processes by port...${NC}"

# Force kill by each port
force_kill_by_port "3000" "SMED Map Viewer"
force_kill_by_port "3003" "Python Service"
force_kill_by_port "3004" "System API Server"
force_kill_by_port "3005" "OpenASP Refactor"
force_kill_by_port "3007" "ASP Manager"
force_kill_by_port "8000" "API Server"

# Cleanup configuration files and old jobs
echo -e "\n${YELLOW}Cleaning up configuration files and database...${NC}"
cd /home/aspuser/app/server/system-cmds
python -c "from functions.job_database import cleanup_old_jobs; print(f'Cleaned up {cleanup_old_jobs()} old jobs')" 2>/dev/null || true

rm -f /home/aspuser/app/.running_services
rm -f /home/aspuser/app/pids/*.pid 2>/dev/null || true

# Wait for process termination
echo -e "\n${YELLOW}Waiting for process termination...${NC}"
sleep 3

# Final port status check
echo -e "\n${YELLOW}Final port status check...${NC}"
all_clear=true

for port in 3000 3003 3005 3007 8000; do
    if lsof -i :$port > /dev/null 2>&1; then
        echo -e "${RED}[WARN] Port $port still in use${NC}"
        all_clear=false
        
        # Final attempt to force kill
        local final_pids=$(lsof -ti:$port 2>/dev/null || true)
        if [ ! -z "$final_pids" ]; then
            echo "  Final force kill attempt..."
            echo "$final_pids" | xargs -r kill -9 2>/dev/null || true
        fi
    else
        echo -e "${GREEN}[OK] Port $port released${NC}"
    fi
done

echo ""
echo "========================================="
if [ "$all_clear" = true ]; then
    echo -e "${GREEN}[DONE] OpenASP AX Development Environment completely shutdown.${NC}"
else
    echo -e "${YELLOW}[WARN] Some ports are still in use.${NC}"
    echo -e "${YELLOW}       System reboot is recommended.${NC}"
fi
echo ""
