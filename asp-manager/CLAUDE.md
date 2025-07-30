# ASP Manager - Claude 작업 히스토리

## 프로젝트 개요
AI 기반 OpenASP 시스템 관리 인터페이스 (포트 3007)

## 주요 기능 구현 내역

### 1. RAG (Retrieval-Augmented Generation) 시스템
- TensorFlow.js + Universal Sentence Encoder 사용
- 실시간 벡터 임베딩 생성
- 코사인 유사도 기반 검색
- PDF 문서 처리 및 임베딩

### 2. AI 채팅 인터페이스
- 컨텍스트 인식 Q&A
- ASP 명령어 및 기능 설명
- 다국어 지원 (한국어/일본어)
- 스트리밍 응답 지원

### 3. 시스템 대시보드
- 실시간 시스템 모니터링
- CPU, 메모리, 디스크 사용률
- 프로세스 관리
- 애니메이션 차트

### 4. ASP WebUI Terminal
- 24x80 가상 터미널 구현
- ASP 명령어 실행 에뮬레이션
- 커서 이동 및 화면 제어
- F1-F12 기능키 지원

### 5. ASP MapEditor
- SMED 파일 시각적 편집
- 드래그앤드롭 필드 배치
- 실시간 미리보기
- JSON 내보내기/가져오기

### 6. 로그 관리 시스템
- 프로그램 실행 추적
- 로그 필터링 및 검색
- 실시간 로그 스트리밍
- 로그 레벨별 색상 코딩

## 기술 스택
- React + TypeScript
- TensorFlow.js
- Express.js (백엔드 프록시)
- WebSocket (실시간 통신)
- IndexedDB (벡터 저장)

## 최근 해결한 이슈
1. PDF 텍스트 추출 최적화
2. 벡터 임베딩 성능 개선
3. 터미널 에뮬레이션 정확도
4. 로그 스트리밍 안정성

## API 엔드포인트
- `/api/chat` - AI 채팅
- `/api/system/status` - 시스템 상태
- `/api/logs` - 로그 조회
- `/api/programs` - 프로그램 관리
- `/api/maps` - SMED 맵 관리

---
최종 업데이트: 2025-07-19