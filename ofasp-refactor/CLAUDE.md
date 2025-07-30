# Claude Code 작업 히스토리

## 프로젝트 개요
- EBCDIC/ASCII 코드 변환 시스템
- Python Flask 백엔드 + React 프론트엔드
- 일본어 더블바이트 문자 및 SOSI 코드 처리

## 현재 구현 상태 (2025-07-19)

### ✅ 완료된 작업
1. **Python 변환 서비스** (`python-service/`)
   - Flask API 서버 (포트: 3003)
   - EBCDIC ↔ ASCII 변환 엔진
   - SOSI 처리 (remove/keep/space)
   - 설정 기반 (CODING_RULES.md 준수)

2. **React 웹 인터페이스** (포트: 3005)
   - ソース変換 페이지: 소스 코드 변환
   - データセット変換 페이지: 데이터셋 변환
   - 배치 변환 기능 (성능 최적화)
   - TypeScript 폴백 시스템

3. **주요 기능**
   - 일본어 Shift-JIS 인코딩 지원
   - SOSI 코드 위치 유지하며 space 변환
   - 배치 처리로 성능 개선 (40+ API 호출 → 4회)
   - 실시간 변환 로그 및 디버깅

### 🔧 기술 스택
- **Backend**: Python 3.10, Flask, Flask-CORS
- **Frontend**: React 18, TypeScript, Tailwind CSS
- **API**: RESTful (포트 3003)
- **데이터**: EBCDIC 코드페이지 테이블

### 📁 주요 파일 구조
```
ofasp-refactor/
├── python-service/              # Flask 백엔드
│   ├── src/api/app.py          # Flask API 서버
│   ├── src/converters/         # 변환 엔진
│   ├── config/config.py        # 환경변수 기반 설정
│   └── convert_file.py         # CLI 도구
├── src/
│   ├── pages/
│   │   ├── SourceConversionPage.tsx    # 소스 변환 UI
│   │   └── DatasetConversionPage.tsx   # 데이터셋 변환 UI
│   ├── utils/
│   │   ├── pythonConverter.ts          # Python API 클라이언트
│   │   └── pythonBatchConverter.ts     # 배치 변환 최적화
│   └── config/api.js                   # API 엔드포인트 설정
└── public/codepages/           # EBCDIC 코드페이지 테이블
```

### 🚀 서비스 시작 방법
```bash
# 1. Python 서비스 시작 (포트 3003)
cd /home/aspuser/app/ofasp-refactor/python-service
FLASK_PORT=3003 python -c "from src.api.app import api; api.run()" &

# 2. React 앱 시작 (포트 3005)
cd /home/aspuser/app/ofasp-refactor
PORT=3005 npm start &
```

### 🧪 테스트 방법
```bash
# CLI 변환 테스트
cd python-service
python convert_file.py /tmp/sample.ebc -e JP -s --sosi-handling space -o /tmp/output.txt

# API 헬스체크
curl http://localhost:3003/health

# 웹 접속
http://localhost:3005
```

### ⚠️ 알려진 이슈
1. **SOSI 처리**: 위치 유지하며 space 변환 구현 완료
2. **성능**: 배치 변환으로 대폭 개선 완료
3. **포트 충돌**: 3003(Python), 3005(React) 사용

### 🔄 최근 변경사항
- Python 서비스 포트를 8001 → 3003으로 변경
- 배치 변환 기능으로 성능 최적화
- SOSI 코드 정확한 위치 처리 구현
- API 설정 파일 업데이트

### 📝 다음 작업 우선순위
1. 데이터셋 변환 페이지 완성
2. 추가 인코딩 타입 지원
3. 에러 핸들링 개선
4. 성능 모니터링 추가

---
**마지막 업데이트**: 2025-07-19
**작업자**: Claude Code Assistant