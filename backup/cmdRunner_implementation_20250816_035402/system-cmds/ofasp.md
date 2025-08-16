
# OpenASP Web Terminal 개발 기록

## 📌 프로젝트 개요
Fujitsu ASP 시스템을 Linux 기반 Open 환경에서 재현하기 위한 프로젝트입니다. SMED 기반 UI, COBOL 연동, 커맨드라인 인터페이스, 웹 터미널, 테스트 시나리오까지 일련의 구성 요소를 개발했습니다.

---

## 🧱 핵심 모듈

- **aspcli.py**: ASP 스타일 커맨드를 처리하는 CLI 및 API 라우터
- **명령어**: CREATLIB, CRTFILE, WRKLIB, WRKVOL, EDTFILE 등 Fujitsu 명령어 호환 구현
- **SMED 맵 렌더링**: JSON 기반 화면 정의, 필드 속성 (PROT, HIDDEN, NUMERIC) 제어
- **웹 터미널**: React + Flask 기반 24x80 고정 터미널 UI 구현

---

## 🖥️ WebTerminal 주요 기능

| 기능                           | 설명 |
|--------------------------------|------|
| 커맨드 라인 입력               | aspcli.py 명령 실행 |
| SMED 맵 출력                   | 필드 렌더링 + 커서 위치 지정 |
| PF키 지원                      | PF1~PF12 커맨드 매핑 |
| TAB/ENTER 이동                 | 입력 필드 순차 포커싱 및 제출 |
| 입력 필드 속성                 | PROT, NUMERIC, HIDDEN, MASK 지원 |
| 테마 전환                      | 다크 ↔ 라이트 모드 |
| 커서 blinking                 | 현재 필드 강조 |
| SMED 전환 시 페이드 애니메이션 | 부드러운 전환 효과 |
| 로딩 인디케이터                | 명령 처리 중 “Please wait...” 표시 |

---

## 💻 통합 소스 코드

첨부된 파일은 다음을 포함합니다:
- `AspCliWebTerminal.tsx`: 웹터미널 UI 구현 TypeScript
- `AspCliWebTerminal.css`: 시각 스타일과 애니메이션 정의

---

## 📦 배포 및 실행

1. `npm install` 후 `react-scripts start`
2. Flask 백엔드 실행 시 `/api/run`, `/api/enter` 엔드포인트로 연동됨
3. 터미널에서 명령 입력 또는 SMED 맵 출력 후 사용

---

## 🔗 회사명/로고

- 브랜드명: **OFASP**
- 하단 표시: *EnduroAX Ltd.*
