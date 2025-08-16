# cmdRunner 구조적 개선 구현 계획

## 1. 아키텍처 개요

### 현재 구조 문제점
- 직접 실행으로 인한 프로세스 관리 부족
- Dataset allocation의 owner 불명확
- 프로세스 종료 시 리소스 정리 미흡
- 동시 실행 프로그램 간 격리 부족

### 개선된 아키텍처
```
OpenASP AX Terminal → API Server → cmdRunner (fork) → CL/PGM (fork)
                                      ↓
                                Dataset Owner
                                - OVRF/DLTOVR 관리
                                - 프로세스 생명주기 추적
                                - 리소스 자동 정리
```

## 2. 핵심 구성 요소

### 2.1 cmdRunner 프로세스 관리자
**파일**: `/home/aspuser/app/server/system-cmds/cmd_runner.py`

**책임**:
- 독립 프로세스로 fork되어 실행
- Dataset allocation/deallocation 관리
- 하위 프로세스 생명주기 추적
- 예외 상황 시 리소스 정리

**주요 기능**:
```python
class CmdRunner:
    def __init__(self, session_id, terminal_id, user_id):
        self.session_id = session_id
        self.terminal_id = terminal_id  
        self.user_id = user_id
        self.pid = os.getpid()
        self.allocated_datasets = {}
        self.child_processes = []
        self.status = "RUNNING"
        
    def execute_command(self, command):
        # CALL 명령어 실행
        
    def allocate_dataset(self, logical_name, physical_path):
        # Dataset 할당 (Owner: cmdRunner PID)
        
    def deallocate_dataset(self, logical_name):
        # Dataset 해제
        
    def cleanup_resources(self):
        # 프로세스 종료 시 모든 리소스 정리
```

### 2.2 Dataset 소유권 관리
**파일**: `/home/aspuser/app/server/system-cmds/dataset_manager.py`

**기능**:
- cmdRunner PID를 Dataset Owner로 등록
- 프로세스별 Dataset 할당 추적
- 자동 리소스 정리

```python
class DatasetManager:
    def __init__(self):
        self.allocations = {}  # {dataset_key: owner_info}
        
    def allocate(self, dataset_key, owner_pid, logical_name, physical_path):
        # cmdRunner를 owner로 dataset 할당
        
    def deallocate(self, dataset_key, owner_pid):
        # owner 검증 후 dataset 해제
        
    def cleanup_by_owner(self, owner_pid):
        # 특정 owner의 모든 dataset 정리
```

### 2.3 프로세스 통신 인터페이스
**파일**: `/home/aspuser/app/server/system-cmds/process_comm.py`

**기능**:
- cmdRunner ↔ API Server 통신
- cmdRunner ↔ CL/PGM 통신
- 상태 보고 및 로깅

## 3. 구현 단계

### Phase 1: 기본 구조 구현
1. **cmdRunner 클래스 구현**
   - 프로세스 생성/관리
   - 기본 명령어 실행
   - 로깅 시스템

2. **Dataset Manager 구현** 
   - 기존 OVRF/DLTOVR 통합
   - Owner 기반 관리
   - 자동 정리 메커니즘

### Phase 2: API Server 통합
1. **API Server 수정**
   - execute_asp_command() 함수 개선
   - cmdRunner fork 로직
   - 세션 관리

2. **프로세스 통신**
   - 상태 보고
   - 에러 처리
   - 로그 통합

### Phase 3: 고급 기능
1. **동시 실행 지원**
   - 다중 cmdRunner 관리
   - 세션별 격리
   - 리소스 충돌 방지

2. **모니터링 & 디버깅**
   - 프로세스 상태 추적
   - 성능 메트릭
   - 디버그 도구

## 4. 호환성 고려사항

### 기존 코드 영향 최소화
- 기존 CALL, OVRF, DLTOVR 함수는 그대로 유지
- cmdRunner가 기존 함수들을 래핑하여 호출
- API 인터페이스 변경 없음

### 점진적 마이그레이션
1. cmdRunner 구현 완료
2. 기존 코드와 병행 운영
3. 안정성 검증 후 완전 전환

## 5. 테스트 계획

### 단위 테스트
- cmdRunner 클래스 테스트
- Dataset Manager 테스트
- 프로세스 통신 테스트

### 통합 테스트  
- CALL CL-CL001.TESTLIB,VOL-DISK01 실행
- 다중 세션 동시 실행
- 예외 상황 처리

### 성능 테스트
- 프로세스 생성 오버헤드 측정
- Dataset 할당/해제 성능
- 메모리 사용량 분석

## 6. 리스크 관리

### 잠재적 리스크
- 프로세스 fork 오버헤드
- 기존 코드와의 호환성
- Dataset 락 데드락 가능성

### 완화 방안
- 성능 최적화
- 철저한 테스트
- 롤백 계획 수립

## 7. 백업 및 복구 계획

### 구현 전 백업
- 전체 시스템 백업
- 주요 설정 파일 백업
- 테스트 환경 구축

### 롤백 전략
- 기존 코드 보존
- 단계별 롤백 포인트
- 긴급 복구 절차