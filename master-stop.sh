#!/bin/bash
# OpenASP AX Complete Development Environment Shutdown Script

# Parse command line options
FORCE_MODE=false
HELP_MODE=false

while [[ $# -gt 0 ]]; do
    case $1 in
        -f|--force)
            FORCE_MODE=true
            shift
            ;;
        -h|--help)
            HELP_MODE=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            HELP_MODE=true
            shift
            ;;
    esac
done

# Color definitions
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Show help if requested
if [ "$HELP_MODE" = true ]; then
    echo -e "${BLUE}OpenASP AX Development Environment Shutdown Script${NC}"
    echo ""
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  -f, --force    Force immediate termination (SIGKILL) without graceful shutdown"
    echo "  -h, --help     Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0             Normal shutdown (graceful termination first)"
    echo "  $0 --force     Force shutdown (immediate SIGKILL)"
    echo ""
    exit 0
fi

if [ "$FORCE_MODE" = true ]; then
    echo -e "${RED}[STOP] OpenASP AX Complete Development Environment FORCE Shutdown...${NC}"
    echo -e "${YELLOW}[WARNING] Using force mode - processes will be killed immediately!${NC}"
else
    echo "[STOP] OpenASP AX Complete Development Environment Shutdown..."
fi
echo "========================================="

# Force kill processes by pattern
force_kill_by_pattern() {
    local pattern="$1"
    local description="$2"
    
    echo -n "$description shutdown... "
    
    if [ "$FORCE_MODE" = true ]; then
        # Force mode: immediate SIGKILL
        pkill -9 -f "$pattern" 2>/dev/null || true
        echo -e "${RED}[FORCE KILLED]${NC}"
    else
        # Normal mode: graceful then force
        pkill -f "$pattern" 2>/dev/null || true
        sleep 2
        
        # Force kill with SIGKILL if still running
        pkill -9 -f "$pattern" 2>/dev/null || true
        echo -e "${GREEN}[OK]${NC}"
    fi
}

# Force kill processes by port
force_kill_by_port() {
    local port="$1"
    local description="$2"
    
    echo -n "$description (Port $port) shutdown... "
    
    # Find processes using the port
    local pids=$(lsof -ti:$port 2>/dev/null || true)
    
    if [ ! -z "$pids" ]; then
        if [ "$FORCE_MODE" = true ]; then
            # Force mode: immediate SIGKILL
            echo "$pids" | xargs -r kill -9 2>/dev/null || true
            echo -e "${RED}[FORCE KILLED]${NC}"
        else
            # Normal mode: graceful then force
            echo "$pids" | xargs -r kill 2>/dev/null || true
            sleep 2
            
            # Force kill if still running
            local remaining_pids=$(lsof -ti:$port 2>/dev/null || true)
            if [ ! -z "$remaining_pids" ]; then
                echo "$remaining_pids" | xargs -r kill -9 2>/dev/null || true
            fi
            echo -e "${GREEN}[OK]${NC}"
        fi
    else
        echo -e "${GREEN}[OK]${NC}"
    fi
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
force_kill_by_pattern "python.*-c.*_job_processor_worker" "Job Processor Services"

echo -e "\n${YELLOW}Force killing processes by port...${NC}"

# Force kill by each port
force_kill_by_port "3000" "SMED Map Viewer"
force_kill_by_port "3003" "Python Service"
force_kill_by_port "3004" "System API Server"
force_kill_by_port "3005" "OpenASP Refactor"
force_kill_by_port "3007" "ASP Manager"
force_kill_by_port "3008" "ASP Manager Backend"
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

for port in 3000 3003 3004 3005 3007 3008 8000; do
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
