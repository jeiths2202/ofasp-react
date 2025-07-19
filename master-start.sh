#!/bin/bash
# OpenASP AX 전체 개발 환경 마스터 시작 스크립트

echo "🚀 OpenASP AX 전체 개발 환경 시작..."
echo "========================================="

# 색상 정의
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# 프로젝트 루트
APP_ROOT="/home/aspuser/app"

# 기존 프로세스 정리
echo -e "${YELLOW}📋 기존 프로세스 정리 중...${NC}"
pkill -f "flask.*3003" 2>/dev/null
pkill -f "react-scripts.*3005" 2>/dev/null
pkill -f "react-scripts.*3007" 2>/dev/null
pkill -f "node.*3008" 2>/dev/null
sleep 3

# 1. Python EBCDIC 변환 서비스 시작 (포트 3003)
echo -e "\n${GREEN}🐍 Python 변환 서비스 시작 (포트 3003)...${NC}"
cd $APP_ROOT/ofasp-refactor/python-service
FLASK_PORT=3003 python -c "from src.api.app import api; api.run()" > /tmp/python-service.log 2>&1 &
PYTHON_PID=$!
echo "Python 서비스 PID: $PYTHON_PID"

# 2. OpenASP Refactor 시작 (포트 3005)
echo -e "\n${GREEN}⚛️  OpenASP Refactor 시작 (포트 3005)...${NC}"
cd $APP_ROOT/ofasp-refactor
PORT=3005 npm start > /tmp/ofasp-refactor.log 2>&1 &
REFACTOR_PID=$!
echo "Refactor 앱 PID: $REFACTOR_PID"

# 3. ASP Manager 백엔드 프록시 시작 (포트 3008)
echo -e "\n${GREEN}🔧 ASP Manager 백엔드 시작 (포트 3008)...${NC}"
cd $APP_ROOT/asp-manager
node server.js > /tmp/asp-manager-backend.log 2>&1 &
BACKEND_PID=$!
echo "백엔드 프록시 PID: $BACKEND_PID"

# 4. ASP Manager 프론트엔드 시작 (포트 3007)
echo -e "\n${GREEN}🎯 ASP Manager 시작 (포트 3007)...${NC}"
cd $APP_ROOT/asp-manager
PORT=3007 npm start > /tmp/asp-manager.log 2>&1 &
MANAGER_PID=$!
echo "Manager 앱 PID: $MANAGER_PID"

# 서비스 시작 대기
echo -e "\n${YELLOW}⏳ 서비스 시작 대기 중 (20초)...${NC}"
for i in {1..20}; do
    echo -n "."
    sleep 1
done
echo ""

# 서비스 상태 확인
echo -e "\n${YELLOW}🔍 서비스 상태 확인...${NC}"
echo "========================================="

# Python 서비스 확인
if curl -s http://localhost:3003/health > /dev/null; then
    echo -e "${GREEN}✅ Python 변환 서비스${NC} - http://localhost:3003"
else
    echo -e "${RED}❌ Python 변환 서비스 시작 실패${NC}"
    echo "   로그 확인: tail -f /tmp/python-service.log"
fi

# OpenASP Refactor 확인
if curl -s http://localhost:3005 > /dev/null; then
    echo -e "${GREEN}✅ OpenASP Refactor${NC} - http://localhost:3005"
else
    echo -e "${RED}❌ OpenASP Refactor 시작 실패${NC}"
    echo "   로그 확인: tail -f /tmp/ofasp-refactor.log"
fi

# ASP Manager 백엔드 확인
if curl -s http://localhost:3008/health > /dev/null; then
    echo -e "${GREEN}✅ ASP Manager 백엔드${NC} - http://localhost:3008"
else
    echo -e "${RED}❌ ASP Manager 백엔드 시작 실패${NC}"
    echo "   로그 확인: tail -f /tmp/asp-manager-backend.log"
fi

# ASP Manager 확인
if curl -s http://localhost:3007 > /dev/null; then
    echo -e "${GREEN}✅ ASP Manager${NC} - http://localhost:3007"
else
    echo -e "${RED}❌ ASP Manager 시작 실패${NC}"
    echo "   로그 확인: tail -f /tmp/asp-manager.log"
fi

# 프로세스 정보 저장
echo -e "\n${YELLOW}💾 프로세스 정보 저장...${NC}"
cat > $APP_ROOT/.running_services << EOF
PYTHON_SERVICE_PID=$PYTHON_PID
REFACTOR_APP_PID=$REFACTOR_PID
BACKEND_PROXY_PID=$BACKEND_PID
MANAGER_APP_PID=$MANAGER_PID
STARTED_AT=$(date)
EOF

echo "========================================="
echo -e "${GREEN}🎉 OpenASP AX 개발 환경 시작 완료!${NC}"
echo ""
echo "📱 주요 서비스 접속 URL:"
echo "   - OpenASP Refactor: http://localhost:3005"
echo "   - ASP Manager: http://localhost:3007"
echo "   - Python API: http://localhost:3003"
echo ""
echo "📋 로그 파일:"
echo "   - Python: /tmp/python-service.log"
echo "   - Refactor: /tmp/ofasp-refactor.log"
echo "   - Manager: /tmp/asp-manager.log"
echo "   - Backend: /tmp/asp-manager-backend.log"
echo ""
echo "🛑 전체 종료 명령어:"
echo "   $APP_ROOT/master-stop.sh"
echo ""