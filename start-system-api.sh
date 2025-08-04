#\!/bin/bash
# Start System API Server on port 3004

echo "Starting System API Server on port 3004..."

cd /home/aspuser/app/ofasp-refactor/server

# Kill any existing process on port 3004
if lsof -ti:3004 >/dev/null 2>&1; then
    echo "Stopping existing service on port 3004..."
    lsof -ti:3004  < /dev/null |  xargs kill -9 2>/dev/null || true
    sleep 2
fi

# Start the system API server
ASPMGR_WEB_PORT=3004 nohup python aspmgr_web.py > /home/aspuser/app/logs/system-api.log 2>&1 &
echo $! > /home/aspuser/app/pids/system-api.pid

echo "System API Server started on port 3004"
echo "PID: $(cat /home/aspuser/app/pids/system-api.pid)"
echo "Log: /home/aspuser/app/logs/system-api.log"
echo "Test: curl http://localhost:3004/api/system"
