# ASP Manager - CODING RULES

이 프로젝트는 `/data/COMMON_CODING_RULES.md`에 정의된 공통 코딩 규칙을 준수합니다.

## 프로젝트 특화 설정

### 환경 변수

```bash
# API 서버 설정
FILE_API_PORT=3008
REACT_APP_PORT=3007
API_BASE_URL=http://localhost:3008

# 파일 시스템 설정
MANUALS_BASE_DIR=/data/asp-manuals
OUTPUT_DIR=/data

# UI 설정
SIDEBAR_WIDTH=256
CHECK_INTERVAL=100

# RAG 시스템 설정
MIN_RELEVANCE_THRESHOLD=0.1
MAX_SEARCH_RESULTS=10
MAX_QUERY_STATS=100

# 다국어 지원
DEFAULT_LANGUAGE=ja
SUPPORTED_LANGUAGES=ja,ko,en
```

### 필수 구성 파일

- `src/config/` - 환경별 설정
- `src/constants/` - 애플리케이션 상수
- `src/i18n/` - 다국어 메시지
- `src/theme/` - UI 테마 설정

모든 하드코딩된 값은 위 파일들로 이동되어야 합니다.