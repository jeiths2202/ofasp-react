#!/bin/bash
# 개발 환경 자동 시작 스크립트

echo "🚀 OpenASP AX 개발 환경 시작..."

# 프로젝트 디렉토리로 이동
cd /home/aspuser/app/ofasp-refactor

# 기존 프로세스 정리
echo "📋 기존 프로세스 정리..."
pkill -f "flask.*3003" 2>/dev/null
pkill -f "react-scripts.*3005" 2>/dev/null
sleep 2

# Python 서비스 시작
echo "🐍 Python Flask 서비스 시작 (포트 3003)..."
cd python-service
FLASK_PORT=3003 python -c "from src.api.app import api; api.run()" &
PYTHON_PID=$!
echo "Python 서비스 PID: $PYTHON_PID"

# React 앱 시작
echo "⚛️ React 앱 시작 (포트 3005)..."
cd /home/aspuser/app/ofasp-refactor
PORT=3005 npm start &
REACT_PID=$!
echo "React 앱 PID: $REACT_PID"

# 서비스 상태 확인
echo "⏳ 서비스 시작 대기 중..."
sleep 10

echo "🔍 서비스 상태 확인..."
if curl -s http://localhost:3003/health > /dev/null; then
    echo "✅ Python 서비스 정상 작동 (http://localhost:3003)"
else
    echo "❌ Python 서비스 시작 실패"
fi

if curl -s http://localhost:3005 > /dev/null; then
    echo "✅ React 앱 정상 작동 (http://localhost:3005)"
else
    echo "❌ React 앱 시작 실패"
fi

echo ""
echo "🎉 개발 환경 시작 완료!"
echo "📱 웹 접속: http://localhost:3005"
echo "🔧 API 서버: http://localhost:3003"
echo ""
echo "프로세스 종료는 다음 명령어로:"
echo "pkill -f 'flask.*3003' && pkill -f 'react-scripts.*3005'"