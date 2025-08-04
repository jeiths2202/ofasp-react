# OpenASP Job Database 조회 가이드

## 데이터베이스 위치
- 경로: `/home/aspuser/app/database/openasp_jobs.db`
- 타입: SQLite3

## 테이블 구조

### jobs 테이블
```sql
job_id          TEXT PRIMARY KEY    -- 작업 ID (예: J1753624628BEEE59)
job_name        TEXT               -- 작업 이름
program         TEXT               -- 실행 프로그램명
library         TEXT               -- 라이브러리명
volume          TEXT               -- 볼륨명 (기본값: DISK01)
parameters      TEXT               -- 실행 매개변수
priority        INTEGER            -- 우선순위 (기본값: 5)
jobq            TEXT               -- 작업 큐 (기본값: @SAME)
jobk            TEXT               -- 작업 키워드
hold            INTEGER            -- 보류 상태 (0/1)
status          TEXT               -- 상태 (PENDING/RUNNING/COMPLETED/ERROR/HELD)
submitted_time  TIMESTAMP          -- 제출 시간
start_time      TIMESTAMP          -- 시작 시간
end_time        TIMESTAMP          -- 종료 시간
pid             INTEGER            -- 프로세스 ID
log_file        TEXT               -- 로그 파일 경로
created_at      TIMESTAMP          -- 생성 시간
updated_at      TIMESTAMP          -- 수정 시간
```

### job_history 테이블
```sql
history_id      INTEGER PRIMARY KEY AUTOINCREMENT
job_id          TEXT               -- 작업 ID (외래키)
status          TEXT               -- 상태 변화
timestamp       TIMESTAMP          -- 변화 시간
message         TEXT               -- 메시지
```

## 접속 방법

### 1. Python을 사용한 접속
```python
import sqlite3

# 데이터베이스 연결
conn = sqlite3.connect('/home/aspuser/app/database/openasp_jobs.db')
cursor = conn.cursor()

# 쿼리 실행
cursor.execute("SELECT * FROM jobs WHERE job_id = ?", ("J1753624628BEEE59",))
result = cursor.fetchone()
print(result)

# 연결 종료
conn.close()
```

### 2. Node.js를 사용한 접속
```javascript
const sqlite3 = require('sqlite3').verbose();

const db = new sqlite3.Database('/home/aspuser/app/database/openasp_jobs.db');

db.get("SELECT * FROM jobs WHERE job_id = ?", ["J1753624628BEEE59"], (err, row) => {
    if (err) {
        console.error(err.message);
    } else {
        console.log(row);
    }
});

db.close();
```

## 주요 쿼리 예제

### 1. JOBID로 특정 작업 검색
```python
python3 -c "
import sqlite3
conn = sqlite3.connect('/home/aspuser/app/database/openasp_jobs.db')
cursor = conn.cursor()
cursor.execute('SELECT job_id, job_name, status, submitted_time FROM jobs WHERE job_id = ?', ('J1753624628BEEE59',))
row = cursor.fetchone()
if row:
    print(f'JOB ID: {row[0]}')
    print(f'JOB NAME: {row[1]}')
    print(f'STATUS: {row[2]}')
    print(f'SUBMITTED: {row[3]}')
else:
    print('Job not found')
conn.close()
"
```

### 2. 특정 상태의 작업들 조회
```bash
# RUNNING 상태의 작업들
python3 -c "
import sqlite3
conn = sqlite3.connect('/home/aspuser/app/database/openasp_jobs.db')
cursor = conn.cursor()
cursor.execute('SELECT job_id, job_name, status FROM jobs WHERE status = ?', ('RUNNING',))
rows = cursor.fetchall()
for row in rows:
    print(f'{row[0]} - {row[1]} - {row[2]}')
conn.close()
"
```

### 3. 최근 작업들 조회 (시간순)
```bash
python3 -c "
import sqlite3
conn = sqlite3.connect('/home/aspuser/app/database/openasp_jobs.db')
cursor = conn.cursor()
cursor.execute('SELECT job_id, job_name, status, submitted_time FROM jobs ORDER BY submitted_time DESC LIMIT 10')
rows = cursor.fetchall()
print('=== 최근 작업 10개 ===')
for row in rows:
    print(f'{row[0]} | {row[1]} | {row[2]} | {row[3]}')
conn.close()
"
```

### 4. 작업 상태별 통계
```bash
python3 -c "
import sqlite3
conn = sqlite3.connect('/home/aspuser/app/database/openasp_jobs.db')
cursor = conn.cursor()
cursor.execute('SELECT status, COUNT(*) FROM jobs GROUP BY status')
rows = cursor.fetchall()
print('=== 작업 상태별 통계 ===')
for row in rows:
    print(f'{row[0]}: {row[1]}개')
conn.close()
"
```

### 5. PID가 있는 실행 중인 작업들
```bash
python3 -c "
import sqlite3
conn = sqlite3.connect('/home/aspuser/app/database/openasp_jobs.db')
cursor = conn.cursor()
cursor.execute('SELECT job_id, job_name, pid, status FROM jobs WHERE pid IS NOT NULL AND status = \"RUNNING\"')
rows = cursor.fetchall()
print('=== PID가 있는 실행 중인 작업들 ===')
for row in rows:
    print(f'{row[0]} | {row[1]} | PID: {row[2]} | {row[3]}')
conn.close()
"
```

### 6. 특정 프로그램을 실행하는 작업들
```bash
python3 -c "
import sqlite3
conn = sqlite3.connect('/home/aspuser/app/database/openasp_jobs.db')
cursor = conn.cursor()
cursor.execute('SELECT job_id, job_name, status FROM jobs WHERE program = ?', ('EMPREAD_CL',))
rows = cursor.fetchall()
print('=== EMPREAD_CL 프로그램 작업들 ===')
for row in rows:
    print(f'{row[0]} | {row[1]} | {row[2]}')
conn.close()
"
```

### 7. 작업 히스토리 조회
```bash
python3 -c "
import sqlite3
conn = sqlite3.connect('/home/aspuser/app/database/openasp_jobs.db')
cursor = conn.cursor()
cursor.execute('SELECT h.job_id, h.status, h.timestamp, h.message FROM job_history h WHERE h.job_id = ? ORDER BY h.timestamp DESC', ('J1753624628BEEE59',))
rows = cursor.fetchall()
print('=== 작업 히스토리 ===')
for row in rows:
    print(f'{row[0]} | {row[1]} | {row[2]} | {row[3]}')
conn.close()
"
```

## 작업 삭제 방법
```bash
# 특정 작업 삭제 (히스토리 포함)
python3 -c "
import sqlite3
conn = sqlite3.connect('/home/aspuser/app/database/openasp_jobs.db')
cursor = conn.cursor()

job_id = 'J1753624628BEEE59'  # 삭제할 작업 ID

# 히스토리 먼저 삭제 (외래키 제약)
cursor.execute('DELETE FROM job_history WHERE job_id = ?', (job_id,))
print(f'Deleted {cursor.rowcount} history records')

# 작업 삭제
cursor.execute('DELETE FROM jobs WHERE job_id = ?', (job_id,))
print(f'Deleted {cursor.rowcount} job records')

conn.commit()
conn.close()
print('작업이 성공적으로 삭제되었습니다.')
"
```

## 유용한 관리 명령들

### 전체 작업 수 조회
```bash
python3 -c "
import sqlite3
conn = sqlite3.connect('/home/aspuser/app/database/openasp_jobs.db')
cursor = conn.cursor()
cursor.execute('SELECT COUNT(*) FROM jobs')
count = cursor.fetchone()[0]
print(f'총 작업 수: {count}개')
conn.close()
"
```

### 오래된 완료 작업들 정리 (7일 이상)
```bash
python3 -c "
import sqlite3
from datetime import datetime, timedelta
conn = sqlite3.connect('/home/aspuser/app/database/openasp_jobs.db')
cursor = conn.cursor()

# 7일 이전 날짜 계산
week_ago = (datetime.now() - timedelta(days=7)).isoformat()

# 오래된 완료 작업들 삭제
cursor.execute('DELETE FROM job_history WHERE job_id IN (SELECT job_id FROM jobs WHERE status IN (\"COMPLETED\", \"ERROR\", \"CANCELLED\") AND datetime(end_time) < ?)', (week_ago,))
cursor.execute('DELETE FROM jobs WHERE status IN (\"COMPLETED\", \"ERROR\", \"CANCELLED\") AND datetime(end_time) < ?', (week_ago,))

print(f'정리된 작업 수: {cursor.rowcount}개')
conn.commit()
conn.close()
"
```