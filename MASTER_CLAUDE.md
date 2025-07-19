# 🚀 OpenASP AX 프로젝트 마스터 문서

> 이 문서는 Claude Code Assistant와 함께 개발한 모든 프로젝트의 통합 히스토리입니다.

## 📋 전체 프로젝트 개요

### 프로젝트 구조
```
/home/aspuser/app/
├── ofasp-refactor/          # 메인 리팩토링 플랫폼 (포트 3005)
├── asp-manager/             # AI 기반 관리 인터페이스 (포트 3007)
├── server/                  # 백엔드 서비스 및 API
│   ├── aspmgr/             # Curses 기반 시스템 관리자
│   ├── api_server.py       # 메인 API 서버
│   └── java_execution/     # OpenASP 프로그램 실행 환경
└── public/                 # 공유 리소스 (코드페이지, 설정 등)
```

## 🎯 각 프로젝트별 상세 정보

### 1. SMED Map Viewer (포트 3000)
**목적**: 레거시 SMED (Screen Map EDitor) 화면 맵 뷰어

#### 주요 기능
- **24x80 터미널 시뮬레이션**: 레거시 터미널 화면 재현
- **SMED 필드 관리**: 필드 위치, 속성, 색상 표시
- **실시간 상호작용**: 입력 필드 값 관리 및 검증
- **화면 제어**: 줌 인/아웃 기능
- **세션 관리**: 사용자 인증 및 프로그램 상태
- **Java 프로그램 연동**: MENU, LOGO, PGM1, PGM2, EIGYO001 등

#### 기술 스택
- React 19 + TypeScript
- 사용자 정의 CSS 그리드 시스템
- JSON 기반 화면 정의

### 2. OpenASP Refactor (포트 3005)
**목적**: 레거시 ASP 시스템을 오픈소스로 마이그레이션하는 웹 기반 플랫폼

#### 주요 기능
- **코드 변환**: COBOL → Java/Python/C/Shell
- **CL 변환**: CL → Shell/JavaScript/Python  
- **EBCDIC 변환**: 
  - ソース変換 (소스 코드 변환)
  - データセット変換 (데이터셋 변환)
  - Python Flask 백엔드 연동 (포트 3003)
  - 배치 처리 최적화
- **AI Transform**: 
  - ASP WebUI Terminal (가상 터미널)
  - ASP MapEditor (SMED 파일 관리)
  - MapLink (맵 연결 시각화)
- **다국어 지원**: 한국어/일본어 UI

#### 기술 스택
- React 19 + TypeScript
- Tailwind CSS
- CodeMirror (구문 하이라이팅)
- i18n (국제화)

### 3. ASP Manager (포트 3007)
**목적**: AI 기반 OpenASP 시스템 관리 및 문서 지원

#### 주요 기능
- **RAG 시스템**: TensorFlow.js 기반 지능형 Q&A
- **PDF 처리**: ASP 매뉴얼 임베딩 및 검색
- **시스템 모니터링**: 실시간 대시보드
- **프로그램 관리**: 실행, 로그, 상태 모니터링
- **SMED 맵 관리**: 시각적 편집기
- **채팅 인터페이스**: AI 어시스턴트

#### 기술 스택
- React + TypeScript
- TensorFlow.js + Universal Sentence Encoder
- Vector Embeddings
- Express.js (백엔드 프록시)

### 4. Python 변환 서비스 (포트 3003)
**목적**: EBCDIC/ASCII 코드 변환 백엔드 서비스

#### 주요 기능
- RESTful API
- SOSI 코드 처리 (remove/keep/space)
- 다중 인코딩 지원 (US, JP, JAK, KEIS, KR)
- 배치 처리 최적화
- 환경변수 기반 설정 (하드코딩 없음)

#### API 엔드포인트
```
GET  /health
GET  /api/v1/info
POST /api/v1/convert/ebcdic-to-ascii
POST /api/v1/convert/ascii-to-ebcdic
```

### 5. ASP CLI 시스템 명령어
**위치**: `/home/aspuser/app/server/system-cmds/`

#### 주요 기능
- ASP 시스템 명령어 재구현 (CLI 버전)
- 볼륨/라이브러리/파일 관리
- 지원 명령어: CRTLIB, DLTLIB, WRKLIB, CRTFILE, DLTFILE, DSPFD, WRKOBJ, WRKVOL 등
- UTF-8 인코딩 지원으로 한국어 메시지 처리
- 테스트 스크립트 제공 (`test1.py`)

#### 사용 예시
```bash
cd /home/aspuser/app/server/system-cmds
python aspcli.py CRTLIB LIB-TESTLIB,VOL-DISK99
python aspcli.py WRKVOL
python test1.py  # 전체 테스트
```

### 6. Curses 시스템 관리자
**위치**: `/home/aspuser/app/server/aspmgr/`

#### 주요 기능
- 터미널 기반 UI
- 시스템 모니터링
- 프로세스 관리
- 로그 뷰어
- Python curses 라이브러리 사용

## 🔧 개발 환경 설정

### 필수 포트
- 3000: SMED Map Viewer (화면 맵 뷰어)
- 3003: Python EBCDIC 변환 서비스
- 3005: OpenASP Refactor 메인
- 3007: ASP Manager
- 3008: ASP Manager 백엔드

### 환경 변수
```bash
# Python 서비스
FLASK_PORT=3003
CODEPAGE_BASE_PATH=/home/aspuser/app/ofasp-refactor/public/codepages

# React 앱들
REACT_APP_PYTHON_CONVERTER_URL=http://localhost:3003
REACT_APP_BASE_URL=http://localhost:3005
REACT_APP_ASP_MANAGER_URL=http://localhost:3007
```

## 📝 최근 작업 내역 (2025년 7월)

### 완료된 주요 작업
1. **EBCDIC 변환 문제 해결**
   - SOSI 코드 위치 유지하며 space 변환
   - 일본어 Shift-JIS 인코딩 처리
   - 0xC2 UTF-8 인코딩 이슈 해결

2. **성능 최적화**
   - 웹 인터페이스 배치 변환 구현
   - API 호출 40+ → 4회로 감소
   - 실시간 진행률 표시

3. **UI/UX 개선**
   - ASP WebUI Terminal 구현 (24x80 가상 터미널)
   - ASP MapEditor (드래그앤드롭 SMED 편집)
   - 로그 관리 시스템 (실행 추적)
   - 다크모드 지원

4. **AI 기능 강화**
   - RAG 기반 문서 검색
   - 실시간 벡터 임베딩
   - 컨텍스트 인식 Q&A

## 🚀 빠른 시작 가이드

### 1. 전체 환경 시작 스크립트
```bash
# 마스터 시작 스크립트 생성 예정
./master-start.sh
```

### 2. 개별 서비스 시작
```bash
# Python 변환 서비스
cd /home/aspuser/app/ofasp-refactor/python-service
FLASK_PORT=3003 python -c "from src.api.app import api; api.run()" &

# OpenASP Refactor
cd /home/aspuser/app/ofasp-refactor
PORT=3005 npm start &

# ASP Manager
cd /home/aspuser/app/asp-manager
PORT=3007 npm start &
```

## 🧪 테스트 및 검증

### EBCDIC 변환 테스트
```bash
cd /home/aspuser/app/ofasp-refactor/python-service
python convert_file.py /tmp/sample.ebc -e JP -s --sosi-handling space -o /tmp/output.txt
```

### API 헬스체크
```bash
curl http://localhost:3003/health
curl http://localhost:3005
curl http://localhost:3007
```

## 📚 개발 규칙

### CODING_RULES.md 주요 내용
1. **하드코딩 금지**: 모든 설정은 환경변수 또는 설정 파일
2. **에러 처리**: 모든 예외는 명시적으로 처리
3. **로깅**: 구조화된 로깅 사용
4. **테스트**: 테스트 코드는 별도 디렉토리
5. **문서화**: 모든 주요 기능은 문서화

## 🔄 작업 재개 방법

### 도커 재시작 후
1. 이 문서 (`MASTER_CLAUDE.md`) 읽기
2. 필요한 서비스 시작
3. 현재 작업 컨텍스트 확인
4. 테스트로 환경 검증

### Claude에게 전달할 내용
```
"도커가 재시작되었습니다. /home/aspuser/app/MASTER_CLAUDE.md를 읽고
전체 OpenASP AX 프로젝트 상태를 파악해주세요. 
주요 서비스: ofasp-refactor(3005), asp-manager(3007), python-service(3003)"
```

## 🎯 다음 작업 우선순위

1. **데이터셋 변환 완성**: 파일 업로드 및 레이아웃 적용
2. **추가 인코딩 지원**: 중국어, 태국어 등
3. **성능 모니터링**: Prometheus/Grafana 통합
4. **보안 강화**: API 인증 및 권한 관리
5. **배포 자동화**: Docker Compose 설정

---
**최종 업데이트**: 2025-07-19
**작업자**: Claude Code Assistant
**사용 언어**: 한국어 우선