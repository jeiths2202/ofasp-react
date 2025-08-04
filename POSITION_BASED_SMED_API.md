# Position-based SMED Rendering API

새로운 Position-based SMED 렌더링 API는 기존의 field name 기반 시스템에서 position-based 시스템으로 전환하여 더 효율적이고 유연한 데이터 렌더링을 제공합니다.

## 주요 특징

- **24x80 그리드 시스템**: 표준 터미널 화면 크기 지원
- **Position-based 매핑**: row, col, length만으로 필드 위치 정의
- **실시간 WebSocket 지원**: 데이터 변경 시 실시간 브로드캐스트
- **완전한 유효성 검증**: 그리드 경계 및 데이터 타입 검증
- **기존 API와 호환**: 기존 field name 기반 API와 병행 사용 가능

## 데이터 구조

### Position Map 형식
```json
[
  {"row": 5, "col": 10, "length": 5},
  {"row": 7, "col": 15, "length": 20},
  {"row": 9, "col": 5, "length": 15}
]
```

### Data Array 형식
```json
["00001", "20250731", "田中太郎"]
```

## API 엔드포인트

### 1. 맵 목록 조회
```http
GET /api/smed/position-render
```

**응답:**
```json
{
  "success": true,
  "maps": [
    {
      "name": "EMPLOYEE_INFO",
      "fields_count": 5,
      "created_at": "2025-07-31T10:00:00",
      "updated_at": "2025-07-31T10:30:00"
    }
  ],
  "total_count": 1
}
```

### 2. 맵 정의 조회
```http
GET /api/smed/position-render/{map_name}
```

**응답:**
```json
{
  "success": true,
  "map_name": "EMPLOYEE_INFO",
  "map": [
    {"row": 0, "col": 0, "length": 20},
    {"row": 2, "col": 5, "length": 10},
    {"row": 3, "col": 5, "length": 15}
  ],
  "created_at": "2025-07-31T10:00:00",
  "updated_at": "2025-07-31T10:30:00"
}
```

### 3. 맵 생성/수정
```http
PUT /api/smed/position-render/{map_name}
```

**요청 본문:**
```json
{
  "map": [
    {"row": 0, "col": 0, "length": 20},
    {"row": 2, "col": 5, "length": 10},
    {"row": 3, "col": 5, "length": 15}
  ]
}
```

**응답:**
```json
{
  "success": true,
  "message": "Position map EMPLOYEE_INFO created successfully",
  "map_name": "EMPLOYEE_INFO",
  "fields_count": 3
}
```

### 4. 데이터 렌더링
```http
POST /api/smed/position-render/{map_name}/data
```

**요청 본문:**
```json
{
  "data": ["사원 정보", "E001234", "김철수"]
}
```

**응답:**
```json
{
  "success": true,
  "map_name": "EMPLOYEE_INFO",
  "grid": [
    "사원 정보              ",
    "                                                                                ",
    "     E001234   ",
    "     김철수           ",
    "                                                                                "
  ],
  "fields": [
    {
      "index": 0,
      "row": 0,
      "col": 0,
      "length": 20,
      "value": "사원 정보",
      "rendered_value": "사원 정보              "
    }
  ],
  "rows": 24,
  "cols": 80
}
```

### 5. 데이터 업데이트 (WebSocket 브로드캐스트)
```http
PUT /api/smed/position-render/{map_name}/data
```

**요청 본문:**
```json
{
  "data": ["사원 정보", "E001234", "김철수"],
  "terminal_id": "terminal_001"
}
```

**응답:**
```json
{
  "success": true,
  "message": "Data updated and broadcasted",
  "map_name": "EMPLOYEE_INFO",
  "grid": [...],
  "fields": [...]
}
```

### 6. 맵 삭제
```http
DELETE /api/smed/position-render/{map_name}
```

**응답:**
```json
{
  "success": true,
  "message": "Position map EMPLOYEE_INFO deleted successfully"
}
```

## WebSocket 이벤트

### 구독하기
```javascript
socket.emit('position_render_subscribe', {
  map_name: 'EMPLOYEE_INFO',
  terminal_id: 'terminal_001'
});
```

### 구독 해제하기
```javascript
socket.emit('position_render_unsubscribe', {
  map_name: 'EMPLOYEE_INFO'
});
```

### 업데이트 수신
```javascript
socket.on('position_render_update', function(data) {
  console.log('Position render update:', data);
  // data.grid: 렌더링된 그리드
  // data.fields: 필드 정보
  // data.map_name: 맵 이름
  // data.terminal_id: 터미널 ID
});
```

## 유효성 검증 규칙

### Position Map 검증
- `row`: 0-23 범위 (24행)
- `col`: 0-79 범위 (80열)  
- `length`: 양수, col + length ≤ 80
- 모든 필드는 정수여야 함

### Data Array 검증  
- 각 데이터 항목은 문자열이어야 함
- 데이터 배열 길이는 맵 배열 길이와 일치해야 함
- 각 데이터 항목 길이는 해당 맵 항목의 length를 초과하면 안됨

## 에러 응답

모든 에러는 다음 형식으로 반환됩니다:
```json
{
  "error": "Error message description"
}
```

**일반적인 에러 코드:**
- `400`: 잘못된 요청 (유효성 검증 실패)
- `404`: 맵을 찾을 수 없음
- `500`: 서버 내부 오류

## 사용 예제

### 1. 기본 사용법
```python
import requests

# 1. 맵 생성
map_data = [
    {"row": 0, "col": 0, "length": 15},   # 제목
    {"row": 2, "col": 5, "length": 10},   # ID
    {"row": 3, "col": 5, "length": 20}    # 이름
]

response = requests.put(
    "http://localhost:8000/api/smed/position-render/USER_INFO",
    json={"map": map_data}
)

# 2. 데이터 렌더링
data = ["사용자 정보", "U001", "홍길동"]
response = requests.post(
    "http://localhost:8000/api/smed/position-render/USER_INFO/data",
    json={"data": data}
)

print(response.json()["grid"])
```

### 2. WebSocket 클라이언트 예제
```javascript
const socket = io('http://localhost:8000');

// 맵 구독
socket.emit('position_render_subscribe', {
  map_name: 'USER_INFO',
  terminal_id: 'web_client'
});

// 업데이트 수신
socket.on('position_render_update', function(data) {
  console.log('받은 그리드 데이터:', data.grid);
  // 화면에 그리드 렌더링
  renderGrid(data.grid);
});
```

## 마이그레이션 가이드

### 기존 Field Name 기반에서 Position 기반으로 전환

**기존 방식:**
```json
{
  "fields": {
    "TITLE": "사용자 정보",
    "USER_ID": "U001",
    "USER_NAME": "홍길동"
  }
}
```

**새로운 방식:**
```json
{
  "map": [
    {"row": 0, "col": 0, "length": 15},
    {"row": 2, "col": 5, "length": 10}, 
    {"row": 3, "col": 5, "length": 20}
  ],
  "data": ["사용자 정보", "U001", "홍길동"]
}
```

### 점진적 전환 전략
1. 기존 API는 그대로 유지
2. 새로운 화면부터 Position-based API 사용
3. 기존 화면은 필요시 단계적으로 전환
4. 두 API는 병행 사용 가능

## 성능 고려사항

- **메모리 효율성**: Position 기반은 필드명 저장이 불필요하여 메모리 효율적
- **렌더링 속도**: 직접적인 위치 계산으로 더 빠른 렌더링
- **네트워크 트래픽**: 더 적은 데이터 전송량
- **확장성**: 새로운 필드 추가 시 맵 정의만 수정하면 됨

## 보안 고려사항

- **입력 검증**: 모든 입력에 대한 철저한 유효성 검증
- **그리드 경계**: 버퍼 오버플로우 방지를 위한 경계 검사
- **데이터 타입**: 엄격한 데이터 타입 검증
- **에러 처리**: 민감한 정보 노출 방지를 위한 안전한 에러 메시지

## 문제 해결

### 자주 발생하는 오류

1. **"Map item extends beyond grid width"**
   - 해결: col + length ≤ 80 확인

2. **"Data array length must match map length"**  
   - 해결: data 배열과 map 배열의 길이를 맞춤

3. **"Row must be between 0-23"**
   - 해결: row 값이 0-23 범위 내인지 확인

4. **"Position map not found"**
   - 해결: PUT 요청으로 맵을 먼저 생성

### 디버깅 팁

- API 서버 로그 확인: `/home/aspuser/app/server/api_server.log`
- 테스트 스크립트 실행: `python /home/aspuser/app/test_position_api.py`
- 헬스 체크: `GET /api/health`

## 향후 계획

- [ ] 영구 저장소 지원 (현재는 메모리만)
- [ ] 맵 템플릿 기능
- [ ] 색상 및 스타일 지원 확장
- [ ] 성능 모니터링 및 최적화
- [ ] 다중 언어 지원 강화