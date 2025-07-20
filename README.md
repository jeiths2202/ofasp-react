# 🚀 OpenASP AX - 레거시 마이그레이션 플랫폼

## 개요
OpenASP AX는 레거시 ASP(Advanced System Products) 시스템을 현대적인 오픈소스 기술로 마이그레이션하는 통합 플랫폼입니다.

## 🏗️ 프로젝트 구성

### 1. [SMED Map Viewer](./) (포트 3000)
- **목적**: 레거시 SMED 화면 맵 뷰어
- **주요 기능**: 24x80 터미널 시뮬레이션, 필드 관리, Java 프로그램 연동
- **기술**: React, TypeScript, CSS Grid

### 2. [OpenASP Refactor](./ofasp-refactor/) (포트 3005)
- **목적**: 코드 변환 및 리팩토링 도구
- **주요 기능**: COBOL/CL 변환, EBCDIC 변환, AI 지원
- **기술**: React, TypeScript, CodeMirror

### 3. [ASP Manager](./asp-manager/) (포트 3007)
- **목적**: AI 기반 시스템 관리 인터페이스
- **주요 기능**: RAG 문서 검색, 시스템 모니터링, 가상 터미널
- **기술**: React, TensorFlow.js, Express.js

### 4. [Python 변환 서비스](./ofasp-refactor/python-service/) (포트 3003)
- **목적**: EBCDIC/ASCII 변환 백엔드
- **주요 기능**: RESTful API, SOSI 처리, 배치 최적화
- **기술**: Python, Flask, Flask-CORS

## 🚀 빠른 시작

### 전체 환경 시작
```bash
./master-start.sh
```

### 전체 환경 종료
```bash
./master-stop.sh
```

### 개별 서비스 시작
```bash
# SMED Map Viewer
npm start

# Python 변환 서비스
cd ofasp-refactor/python-service
FLASK_PORT=3003 python -c "from src.api.app import api; api.run()"

# OpenASP Refactor
cd ofasp-refactor
PORT=3005 npm start

# ASP Manager
cd asp-manager
PORT=3007 npm start
```

## 📋 주요 문서

- [MASTER_CLAUDE.md](./MASTER_CLAUDE.md) - 전체 프로젝트 작업 히스토리
- [PROJECT_CONTEXT.json](./PROJECT_CONTEXT.json) - 구조화된 프로젝트 정보
- [CODING_RULES.md](./ofasp-refactor/CODING_RULES.md) - 개발 규칙 및 표준

## 🧪 테스트

### EBCDIC 변환 테스트
```bash
cd ofasp-refactor/python-service
python convert_file.py /tmp/sample.ebc -e JP -s --sosi-handling space -o /tmp/output.txt
```

### API 상태 확인
```bash
curl http://localhost:3000         # SMED Viewer
curl http://localhost:3003/health  # Python 서비스
curl http://localhost:3005         # Refactor 앱
curl http://localhost:3007         # Manager 앱
```

## 🔧 개발 환경

### 필수 요구사항
- Node.js 18+
- Python 3.10+
- npm 또는 yarn

### 환경 변수
```bash
FLASK_PORT=3003
REACT_APP_PYTHON_CONVERTER_URL=http://localhost:3003
CODEPAGE_BASE_PATH=/home/aspuser/app/ofasp-refactor/public/codepages
```

### 문자 인코딩 및 국제화 규칙

#### SJIS 인코딩 사용
- **일본어 환경 지원**: ja_JP.sjis 로케일 환경에서의 호환성을 위해 스크립트 파일은 SHIFT_JIS 인코딩으로 작성해야 합니다.
- **적용 대상**: Shell 스크립트 (.sh), 배치 파일, 설정 파일 등 시스템 레벨 파일
- **변환 방법**: UTF-8로 작성 후 SHIFT_JIS로 변환 (이모지 제거 필요)

#### 이모지 사용 금지
- **모든 소스 코드**: 소스 코드, 주석, 문서에서 이모지 사용을 금지합니다.
- **대체 표기**: 이모지 대신 ASCII 문자 조합을 사용합니다.
  ```bash
  # 금지: 🚀 시작, ✅ 성공, ❌ 실패, 📝 메모, 🔧 설정
  # 권장: [START], [OK], [NG], [NOTE], [CONFIG]
  ```
- **예외 사항**: UI 텍스트에서는 사용자 경험을 위해 제한적 허용
- **이유**: 
  - SHIFT_JIS 인코딩에서 이모지 지원 불가
  - 크로스 플랫폼 호환성 보장
  - 코드 가독성 및 전문성 유지

#### 주석 작성 가이드라인
```python
# English comments only - all source code comments must be in English
def process_data(input_file):
    """
    Process input file and return results.
    
    Args:
        input_file (str): Path to input file
        
    Returns:
        dict: Processed data results
    """
    # Initialize data structure
    result = {}
    
    # Process each line in the file
    with open(input_file, 'r') as f:
        for line in f:
            # Skip empty lines and comments
            if not line.strip() or line.startswith('#'):
                continue
                
    return result
```

#### 인코딩 변환 예시
```bash
# UTF-8 → SHIFT_JIS 변환 (이모지 제거 포함)
python3 -c "
with open('script.sh', 'r', encoding='utf-8') as f:
    content = f.read()
# Remove emojis and replace with ASCII alternatives
content = content.replace('🚀', '[START]').replace('✅', '[OK]').replace('❌', '[NG]')
with open('script.sh', 'w', encoding='shift_jis') as f:
    f.write(content)
"
```

## 📁 디렉토리 구조
```
/home/aspuser/app/
├── ofasp-refactor/          # 메인 리팩토링 플랫폼
│   ├── src/                 # React 소스 코드
│   ├── python-service/      # Python 백엔드
│   └── public/             # 정적 리소스
├── asp-manager/            # AI 관리 인터페이스
│   ├── src/                # React 소스 코드
│   └── server.js          # Express 프록시
├── server/                 # 백엔드 서비스
│   └── aspmgr/            # Curses 시스템 관리자
├── master-start.sh        # 전체 시작 스크립트
└── master-stop.sh         # 전체 종료 스크립트
```

## 📋 개발 규칙 및 가이드라인

### 코딩 표준
- **언어별 규칙**: [CODING_RULES.md](./ofasp-refactor/CODING_RULES.md) 참조
- **문자 인코딩**: 시스템 스크립트는 SHIFT_JIS, 소스 코드는 UTF-8
- **주석 언어**: 모든 소스 코드 내 주석은 영어로 작성
- **이모지 금지**: 모든 소스 코드 및 시스템 파일에서 이모지 사용 금지

### 파일 생성 시 주의사항
1. **Shell 스크립트 (.sh)**
   - UTF-8로 작성 후 SHIFT_JIS로 변환
   - 이모지 사용 금지, ASCII 대체 문자 사용 (`[START]`, `[OK]`, `[NG]`)
   - 모든 주석은 영어로 작성
   - 메시지 출력도 영어 권장 (국제화 고려)

2. **Python 스크립트 (.py)**
   - UTF-8 인코딩 유지
   - 파일 상단에 `# -*- coding: utf-8 -*-` 선언
   - 모든 주석과 docstring은 영어로 작성
   - 이모지 사용 금지
   - SJIS 변환이 필요한 출력은 런타임에 처리

3. **JavaScript/TypeScript (.js/.ts/.tsx)**
   - UTF-8 인코딩 사용
   - 모든 주석은 영어로 작성
   - 이모지 사용 금지 (UI 텍스트 제외)
   - JSDoc 주석도 영어로 작성

4. **설정 파일**
   - JSON: UTF-8 인코딩, 주석 불가하지만 키명은 영어
   - 시스템 설정: SHIFT_JIS 고려
   - YAML/XML: UTF-8 인코딩, 주석은 영어

### 국제화 지원
- **로케일 지원**: ja_JP.sjis, en_US.UTF-8
- **메시지 표시**: 환경에 따른 인코딩 자동 감지
- **폰트 지원**: 일본어 표시 가능한 터미널 폰트 사용

## 🤝 기여 방법

1. 이슈 생성
2. 기능 브랜치 생성
3. 변경사항 커밋 (인코딩 규칙 준수)
4. 풀 리퀘스트 생성
5. 코드 리뷰 및 머지

## 📝 라이선스

이 프로젝트는 내부 사용을 위해 개발되었습니다.

## 🔖 빠른 참조

### 인코딩 변환 스크립트
```bash
# SJIS 변환용 Python 스크립트 실행
python3 /tmp/convert_to_sjis.py

# 수동 변환
iconv -f UTF-8 -t SHIFT_JIS input.sh > output.sh
```

### 주요 명령어
```bash
# 전체 환경 관리
./master-start.sh    # 모든 서비스 시작
./master-stop.sh     # 모든 서비스 정지

# 개별 서비스 확인
curl http://localhost:3000  # SMED Viewer
curl http://localhost:3003  # Python Service  
curl http://localhost:3005  # OFASP Refactor
curl http://localhost:3007  # ASP Manager
curl http://localhost:8000  # API Server

# 로그 확인
tail -f logs/python-service.log
tail -f logs/smed-viewer.log
tail -f logs/ofasp-refactor.log
tail -f logs/asp-manager.log
tail -f logs/api-server.log
```

### 문제 해결
- **문자 깨짐**: LANG=ja_JP.sjis 환경에서 SHIFT_JIS 인코딩 확인
- **포트 충돌**: `./master-stop.sh` 실행 후 `netstat -an | grep 300` 확인
- **서비스 미시작**: 로그 파일에서 에러 메시지 확인

---
**개발**: Claude Code Assistant와 함께  
**최종 업데이트**: 2025-07-20  
**인코딩**: SHIFT_JIS (시스템 스크립트), UTF-8 (소스 코드)