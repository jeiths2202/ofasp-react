# 🚀 OpenASP AX - 레거시 마이그레이션 플랫폼

## ⚠️ **모든 AI CODING AGENT 필수 준수사항**

### 🔥 **절대 금지 원칙 - NEVER HARDCODE**
```
❌ 절대 금지: "데모용", "임시로", "일단", "테스트용" 등의 핑계로 하드코딩
❌ 절대 금지: 실제 파일 시스템 대신 목업(mock) 데이터 사용
❌ 절대 금지: 에러 발생 시 우회 처리 (반드시 근본 원인 해결)
❌ 절대 금지: 사용자 요구 파일 수와 다른 임의의 파일 수 처리

✅ 필수 원칙: 모든 데이터는 실제 시스템에서 동적으로 로드
✅ 필수 원칙: 사용자가 요구한 정확한 파일 수 처리 (1개도 빠짐없이)
✅ 필수 원칙: 실제 파일 시스템과 화면 표시 완전 일치
✅ 필수 원칙: 에러는 반드시 해결, 절대 우회 금지
```

### 📊 **실제 사례: 하드코딩으로 인한 심각한 문제**
- **문제 상황**: AI Transform 페이지에서 실제 1,022개 파일 중 14개만 하드코딩으로 표시
- **파급 효과**: 98.6% 기능 누락, 실제 업무 사용 불가능
- **근본 원인**: "데모용이니까 하드코딩해도 된다"는 잘못된 사고방식
- **교훈**: 이 프로젝트는 실제 레거시 마이그레이션용 업무 도구임

### 🛡️ **재발방지 검증 체계**
1. **파일 수 검증**: `find` 명령어 결과와 화면 표시 반드시 일치
2. **내용 검증**: 실제 파일 내용과 화면 표시 내용 완전 일치
3. **성능 검증**: 1,000+ 파일도 정상 처리 확인
4. **에러 해결**: 500 에러 등 모든 에러는 근본 해결

### 🔧 **올바른 구현 방법**
- **백엔드 API**: Python Flask에 실제 파일 스캐닝 엔드포인트 구현
- **대용량 처리**: 스트리밍, 페이지네이션으로 1,000+ 파일 처리
- **실시간 연동**: 실제 파일 시스템 변경사항 즉시 반영
- **완전한 기능**: 사용자 요구사항 100% 구현

---

## 개요
OpenASP AX는 레거시 ASP(Advanced System Products) 시스템을 현대적인 오픈소스 기술로 마이그레이션하는 **실제 업무용** 통합 플랫폼입니다.

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

### 🚨 **최우선 원칙: 하드코딩 절대 금지**
```
이 프로젝트는 실제 레거시 시스템을 처리하는 업무용 도구입니다.
"데모용", "임시", "테스트" 등의 핑계로 하드코딩하는 것은 절대 금지됩니다.

모든 데이터는 실제 파일 시스템에서 동적으로 로드해야 합니다.
사용자가 요구한 파일 수와 화면 표시가 1개라도 다르면 안됩니다.
에러가 발생하면 우회하지 말고 반드시 근본 원인을 해결하세요.
```

### 코딩 표준
- **하드코딩 금지**: 모든 데이터는 실제 시스템에서 동적 로드 **[최우선]**
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

### 🔥 **작업 전 필수 체크리스트**
- [ ] README.md의 "하드코딩 절대 금지" 원칙 숙지
- [ ] 실제 파일 시스템과 연동하여 구현할 계획 수립
- [ ] 사용자 요구 파일 수와 구현 예정 파일 수 일치 확인
- [ ] 에러 발생 시 근본 해결 방안 준비

### 작업 프로세스
1. **필수 체크리스트 완료**
2. 이슈 생성
3. 기능 브랜치 생성
4. **실제 데이터 연동 구현** (하드코딩 절대 금지)
5. 변경사항 커밋 (인코딩 규칙 준수)
6. **파일 수 일치 검증**
7. 풀 리퀘스트 생성
8. 코드 리뷰 및 머지

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

## 📋 프로그램 등록 및 catalog.json 관리

### 🔧 **프로그램 등록 필수 조건**
```
⚠️ 중요: OpenASP에서 모든 프로그램은 실행 전에 catalog.json에 등록되어야 합니다.
등록되지 않은 프로그램은 CALL 명령어로 실행할 수 없습니다.
```

### 📝 catalog.json 프로그램 등록 형식

#### Java 프로그램 등록
```json
{
  "DISK01": {
    "TESTLIB": {
      "CUINP001": {
        "TYPE": "PGM",
        "PGMTYPE": "JAVA",
        "PGMNAME": "CUINP001",
        "CLASSFILE": "CUINP001.class",
        "DESCRIPTION": "Customer data input program for FB format SAM files",
        "VERSION": "1.0",
        "CREATED": "2025-07-24T17:04:00.000000Z",
        "UPDATED": "2025-07-24T17:04:00.000000Z"
      }
    }
  }
}
```

#### COBOL 프로그램 등록
```json
{
  "PAYROLL01": {
    "TYPE": "PGM",
    "PGMTYPE": "COBOL",
    "PGMNAME": "PAYROLL01",
    "SOURCEFILE": "PAYROLL01.cbl",
    "EXECUTABLE": "PAYROLL01",
    "DESCRIPTION": "Monthly payroll calculation",
    "VERSION": "2.1",
    "CREATED": "2025-07-21T10:00:00Z",
    "UPDATED": "2025-07-21T10:00:00Z"
  }
}
```

#### Shell 프로그램 등록
```json
{
  "test_shell": {
    "TYPE": "PGM",
    "PGMTYPE": "SHELL",
    "PGMNAME": "test_shell.sh",
    "SHELLFILE": "test_shell.sh",
    "DESCRIPTION": "Test shell program",
    "VERSION": "1.0",
    "CREATED": "2025-07-21T09:46:17.516107Z",
    "UPDATED": "2025-07-21T09:46:17.516107Z"
  }
}
```

### 🎯 **프로그램 등록 시 필수 필드**
- `TYPE`: 반드시 "PGM"
- `PGMTYPE`: "JAVA", "COBOL", "SHELL" 중 하나
- `PGMNAME`: 실제 프로그램명
- `DESCRIPTION`: 프로그램 설명
- `VERSION`: 버전 정보
- `CREATED`, `UPDATED`: ISO 형식 타임스탬프

### 🚀 **프로그램 등록 후 실행 방법**
```bash
# CL 명령어로 프로그램 실행
CALL PGM-CUINP001.TESTLIB,PARA='001,ABC',VOL-DISK01

# ASP 명령어로 직접 실행  
CALL PGM-CUINP001.TESTLIB,PARA-(001,ABC),VOL-DISK01
```

### 📋 **관련 파일 위치**
- **catalog.json**: `/home/aspuser/app/asp-manager/public/config/catalog.json`
- **프로그램 파일**: `/home/aspuser/app/volume/DISK01/TESTLIB/`
- **CL Parser**: `/home/aspuser/app/server/system-cmds/cl_parser.py`
- **CL Executor**: `/home/aspuser/app/server/system-cmds/cl_executor.py`
- **서비스 미시작**: 로그 파일에서 에러 메시지 확인

---

## 🔖 **모든 AI AGENT 기억해야 할 핵심 원칙**

```
이 프로젝트는 장난감이 아닙니다.
실제 레거시 시스템을 마이그레이션하는 업무용 도구입니다.

절대로 하드코딩하지 마세요.
절대로 "데모용"이라는 핑계를 대지 마세요.
절대로 사용자 요구사항을 임의로 축소하지 마세요.
절대로 에러를 우회하지 말고 반드시 해결하세요.

실제 파일 시스템의 모든 파일을 정확히 처리하세요.
1개 파일이라도 빠뜨리면 안됩니다.
```

**개발**: Claude Code Assistant와 함께  
**최종 업데이트**: 2025-07-23  
**핵심 원칙**: 하드코딩 절대 금지, 실제 데이터만 사용  
**인코딩**: SHIFT_JIS (시스템 스크립트), UTF-8 (소스 코드)