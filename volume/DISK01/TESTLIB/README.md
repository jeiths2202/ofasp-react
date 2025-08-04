# OpenASP Web Terminal with MSGSAMPLEBROWSERMENU Support

웹터미널에서 MSGSAMPLEBROWSERMENU 출력을 처리하는 완전한 React 기반 터미널 시스템입니다.

## 🚀 주요 기능

### 1. **웹소켓 기반 실시간 터미널**
- 실시간 명령어 실행 및 출력 표시
- 자동 재연결 기능
- 연결 상태 실시간 모니터링

### 2. **MSGSAMPLEBROWSERMENU 통합**
- Java 프로그램이 출력하는 JSON 응답 자동 파싱
- Employee 레코드를 테이블 형태로 실시간 표시
- 페이지네이션 및 검색 기능 지원

### 3. **사용자 인증 시스템**
- 워크스테이션 기반 인증
- 세션 관리 및 지속 시간 추적
- 보안 로그아웃 기능

### 4. **반응형 UI 디자인**
- 터미널 스타일의 모던한 인터페이스
- 실시간 애니메이션 효과
- 키보드 단축키 지원

## 📁 파일 구조

```
/home/aspuser/app/volume/DISK01/TESTLIB/
├── React Components
│   ├── App.tsx                    # 메인 애플리케이션 컴포넌트
│   ├── WorkstationAuthWrapper.tsx # 인증 래퍼 컴포넌트
│   ├── AspCliWebTerminal.tsx      # 터미널 메인 컴포넌트
│   └── EmployeeDataViewer.tsx     # Employee 데이터 뷰어
├── Services
│   ├── websocketService.ts        # WebSocket 통신 서비스
│   └── types.ts                   # TypeScript 타입 정의
├── Java Backend
│   ├── MSGSampleBrowserMenuJSON.java # JSON 출력 생성기
│   └── EMPLOYEE_TEST.FB           # 테스트 데이터 파일
├── Demo & Config
│   ├── index.html                 # 완전한 데모 페이지
│   ├── package.json               # NPM 설정
│   └── README.md                  # 이 문서
```

## 🛠️ 기술 스택

- **Frontend**: React 18 + TypeScript
- **Styling**: CSS-in-JS (Styled Components)
- **Real-time Communication**: WebSocket API
- **Backend**: Java (MSGSAMPLEBROWSERMENU)
- **Data Format**: JSON

## 🚀 빠른 시작

### 1. 데모 실행
```bash
# 브라우저에서 index.html을 열어 완전한 데모 확인
open index.html
```

### 2. Java 백엔드 테스트
```bash
# Java 프로그램 컴파일 및 실행
javac MSGSampleBrowserMenuJSON.java
java MSGSampleBrowserMenuJSON
```

### 3. 개발 환경 설정
```bash
# 의존성 설치
npm install

# 개발 서버 시작
npm start

# TypeScript 컴파일 확인
npm run compile
```

## 💡 사용법

### 1. **인증 과정**
1. 웹페이지 접속 후 로그인 화면 표시
2. Username, Workstation, Password 입력 (데모에서는 임의 값 허용)
3. "Login" 버튼 클릭하여 인증 완료

### 2. **터미널 연결**
1. "Connect" 버튼 클릭하여 WebSocket 연결
2. 연결 상태가 "CONNECTED"로 변경 확인
3. 터미널 입력창 활성화

### 3. **MSGSAMPLEBROWSERMENU 실행**
1. **방법 1**: "Employee Browser" 퀵 버튼 클릭
2. **방법 2**: 터미널에서 직접 명령어 입력
   ```
   > CALL MSGSAMPLEBROWSERMENU
   > MSGSAMPLEBROWSERMENU
   ```

### 4. **Employee 데이터 확인**
1. 명령어 실행 후 자동으로 Employee Data Viewer 팝업 표시
2. 테이블 형태로 정리된 직원 정보 확인
3. ESC 키 또는 Close 버튼으로 팝업 닫기

## 🔧 주요 컴포넌트 설명

### WorkstationAuthWrapper
- 최상위 인증 관리 컴포넌트
- 로그인 폼 및 세션 관리
- 인증 성공 시 터미널 컴포넌트 렌더링

### AspCliWebTerminal
- 메인 터미널 인터페이스
- WebSocket 연결 관리
- 명령어 입력/출력 처리
- 퀵 명령어 버튼 제공

### EmployeeDataViewer
- SMED Map 데이터 전용 표시 컴포넌트
- 테이블 형태의 데이터 렌더링
- 실시간 애니메이션 효과
- 키보드 단축키 지원

### WebSocketService
- WebSocket 통신 관리 클래스
- 자동 재연결 기능
- 메시지 타입별 라우팅
- 오류 처리 및 상태 관리

## 📊 JSON 데이터 형식

### SMED Map Response
```json
{
  "type": "smed_map",
  "map_name": "BROWSE_MENU",
  "title": "==== Employee Data List ====",
  "subtitle": "Employee Data Browser",
  "headers": ["EmpID", "Name", "Dept", "Salary", "HireDate", "Status"],
  "page_info": { "current": 1, "total": 1 },
  "function_keys": "F1=Prev F2=Next F3=Quit",
  "status": "Data displayed",
  "data": [
    {
      "id": "100001",
      "name": "John Smith",
      "dept": "IT",
      "salary": "$3,500",
      "hire_date": "2022-01-01",
      "status": "ACTIVE"
    }
  ]
}
```

## 🎨 UI 특징

### 터미널 스타일
- 검은 배경 (#0c0c0c)
- 녹색 텍스트 (#00ff00)
- 모노스페이스 폰트
- 실시간 스크롤링

### 색상 체계
- **입력**: 노란색 (#ffff00)
- **출력**: 녹색 (#00ff00)  
- **오류**: 빨간색 (#ff6060)
- **시스템**: 청록색 (#00ffff)

### 애니메이션
- 페이드인 효과
- 호버 상태 변화
- 연결 상태 표시기
- 로딩 스피너

## 🔒 보안 고려사항

### 인증
- 세션 기반 사용자 관리
- 워크스테이션 식별자 검증
- 자동 로그아웃 기능

### 통신
- WebSocket 보안 연결 지원
- 메시지 유효성 검사
- 오류 처리 및 복구

## 🚨 오류 처리

### WebSocket 연결
- 자동 재연결 시도 (최대 5회)
- 연결 상태 실시간 표시
- 오류 메시지 표시

### 데이터 파싱
- JSON 형식 유효성 검사
- 필수 필드 존재 확인
- 오류 시 기본값 처리

## 📈 성능 최적화

### React 최적화
- useCallback으로 함수 메모이제이션
- useMemo로 계산 결과 캐싱
- 불필요한 리렌더링 방지

### 메모리 관리
- WebSocket 연결 정리
- 이벤트 리스너 해제
- 타이머 정리

## 🤝 확장성

### 새로운 명령어 추가
1. `websocketService.ts`에 새 메시지 타입 정의
2. `AspCliWebTerminal.tsx`에 핸들러 추가
3. 필요 시 전용 뷰어 컴포넌트 생성

### 테마 시스템
- `types.ts`의 `TerminalTheme` 인터페이스 활용
- CSS 변수를 통한 동적 테마 변경
- 사용자 설정 저장 기능

## 📝 개발 노트

### TypeScript 사용
- 엄격한 타입 체크
- 인터페이스 기반 설계
- 런타임 오류 방지

### 코드 구조
- 컴포넌트별 단일 책임 원칙
- 서비스 레이어 분리
- 재사용 가능한 유틸리티

## 🎯 향후 개선 계획

### 기능 확장
- [ ] 다중 페이지 지원
- [ ] 실시간 검색 기능
- [ ] 데이터 정렬 기능
- [ ] CSV 내보내기

### UX 개선
- [ ] 드래그 앤 드롭 지원
- [ ] 키보드 네비게이션
- [ ] 접근성 향상
- [ ] 모바일 최적화

### 성능 향상
- [ ] 가상 스크롤링
- [ ] 데이터 페이지네이션
- [ ] 이미지 레이지 로딩
- [ ] 번들 사이즈 최적화

---

**© 2024 OpenASP Development Team**  
*완전한 웹 터미널 솔루션으로 MSGSAMPLEBROWSERMENU를 포함한 모든 SAM FILE 작업을 지원합니다.*