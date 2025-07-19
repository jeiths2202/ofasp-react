#!/bin/bash
# OpenASP AX 전체 개발 환경 종료 스크립트

echo "🛑 OpenASP AX 전체 개발 환경 종료..."
echo "========================================="

# 색상 정의
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# 저장된 PID 파일 읽기
if [ -f /home/aspuser/app/.running_services ]; then
    source /home/aspuser/app/.running_services
    echo -e "${GREEN}저장된 프로세스 정보를 찾았습니다.${NC}"
else
    echo -e "${YELLOW}저장된 프로세스 정보가 없습니다. 프로세스 이름으로 종료합니다.${NC}"
fi

# 프로세스 종료
echo -e "\n${YELLOW}프로세스 종료 중...${NC}"

# SMED Map Viewer 종료
echo -n "SMED Map Viewer 종료... "
pkill -f "react-scripts.*3000" 2>/dev/null
if [ ! -z "$SMED_VIEWER_PID" ]; then
    kill $SMED_VIEWER_PID 2>/dev/null
fi
echo -e "${GREEN}✓${NC}"

# Python 서비스 종료
echo -n "Python 변환 서비스 종료... "
pkill -f "flask.*3003" 2>/dev/null
if [ ! -z "$PYTHON_SERVICE_PID" ]; then
    kill $PYTHON_SERVICE_PID 2>/dev/null
fi
echo -e "${GREEN}✓${NC}"

# OpenASP Refactor 종료
echo -n "OpenASP Refactor 종료... "
pkill -f "react-scripts.*3005" 2>/dev/null
if [ ! -z "$REFACTOR_APP_PID" ]; then
    kill $REFACTOR_APP_PID 2>/dev/null
fi
echo -e "${GREEN}✓${NC}"

# ASP Manager 백엔드 종료
echo -n "ASP Manager 백엔드 종료... "
pkill -f "node.*server.js" 2>/dev/null
if [ ! -z "$BACKEND_PROXY_PID" ]; then
    kill $BACKEND_PROXY_PID 2>/dev/null
fi
echo -e "${GREEN}✓${NC}"

# ASP Manager 종료
echo -n "ASP Manager 종료... "
pkill -f "react-scripts.*3007" 2>/dev/null
if [ ! -z "$MANAGER_APP_PID" ]; then
    kill $MANAGER_APP_PID 2>/dev/null
fi
echo -e "${GREEN}✓${NC}"

# 관련 노드 프로세스 정리
echo -n "관련 노드 프로세스 정리... "
pkill -f "webpack-dev-server" 2>/dev/null
pkill -f "fork-ts-checker" 2>/dev/null
echo -e "${GREEN}✓${NC}"

# PID 파일 삭제
rm -f /home/aspuser/app/.running_services

# 포트 확인
echo -e "\n${YELLOW}포트 상태 확인...${NC}"
for port in 3000 3003 3005 3007 3008; do
    if lsof -i :$port > /dev/null 2>&1; then
        echo -e "${RED}⚠️  포트 $port 아직 사용 중${NC}"
    else
        echo -e "${GREEN}✓ 포트 $port 해제됨${NC}"
    fi
done

echo ""
echo "========================================="
echo -e "${GREEN}🎉 OpenASP AX 개발 환경이 종료되었습니다.${NC}"
echo ""