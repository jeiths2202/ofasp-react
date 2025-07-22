# WebTerminal과 SMED Maps 연동 기능

## 개요
OpenASP React 프로젝트의 핵심 기능 중 하나인 WebTerminal과 SMED(Screen Map Editor) Maps의 완전한 연동 시스템에 대한 상세 가이드입니다.

## 시스템 아키텍처

### 1. 전체 구조
```
WebTerminal ←→ Flask API Server ←→ Java Programs ←→ SMED Files
    ↑                ↑                    ↑             ↑
React UI      Python Backend      JAR Executor    SJIS Files
```

### 2. 주요 구성 요소
- **WebTerminal**: React 기반 터미널 에뮬레이터
- **Flask API Server**: Python 백엔드 서버 
- **Java Program Executor**: JAR 파일 실행 엔진
- **SMED Files**: SJIS 인코딩된 화면 맵 파일들
- **Function Key Handler**: F1-F12 키 이벤트 처리

## 구현 세부사항

### 1. Flask API Server
**파일**: `/home/aspuser/app/server/api_server.py`

#### CALL 명령어 처리
```python
@app.route('/api/call', methods=['POST'])
def call_program():
    try:
        data = request.get_json()
        command = data.get('command', '')
        
        # CALL PGM-TestProgram.TESTLIB,VOL-DISK01 파싱
        if command.startswith('CALL '):
            program_spec = command[5:].strip()
            pgm_part, vol_part = program_spec.split(',')
            
            # PGM-TestProgram.TESTLIB 파싱
            pgm_name = pgm_part.split('-')[1].split('.')[0]
            library = pgm_part.split('.')[1]
            volume = vol_part.split('-')[1]
            
            # Java 프로그램 실행
            result = execute_java_program(pgm_name, volume, library)
            
            return jsonify({
                'success': True,
                'output': result.get('output', ''),
                'smed_content': result.get('smed_content', ''),
                'encoding': 'utf-8'
            })
            
    except Exception as e:
        return jsonify({'success': False, 'error': str(e)}), 500
```

#### Java 프로그램 실행기
```python
def execute_java_program(pgm_name, volume, library):
    try:
        # JAR 파일 경로
        jar_path = "/home/aspuser/app/server/java_jars/ofasp.jar"
        
        # Java 명령어 구성
        java_cmd = [
            'java', '-cp', jar_path,
            f'com.openasp.{library.lower()}.{pgm_name}',
            '--volume', volume,
            '--library', library
        ]
        
        # 프로세스 실행
        result = subprocess.run(
            java_cmd, 
            capture_output=True, 
            text=True, 
            timeout=30,
            encoding='utf-8'
        )
        
        output = result.stdout
        
        # SMED 맵 정보 추출
        smed_map = extract_smed_map_from_output(output)
        smed_content = ''
        
        if smed_map:
            smed_content = load_smed_file(smed_map, volume, library)
            
        return {
            'output': output,
            'smed_content': smed_content,
            'exit_code': result.returncode
        }
        
    except subprocess.TimeoutExpired:
        return {'error': 'Program execution timeout', 'exit_code': -1}
    except Exception as e:
        return {'error': str(e), 'exit_code': -1}
```

#### SMED 맵 로딩
```python
def load_smed_file(map_name, volume, library):
    try:
        file_path = f"/home/aspuser/app/volume/{volume}/{library}/{map_name}"
        
        # SJIS 인코딩으로 파일 읽기
        with open(file_path, 'r', encoding='shift_jis', errors='replace') as f:
            content = f.read()
            
        return content
        
    except FileNotFoundError:
        logger.warning(f"SMED file not found: {file_path}")
        return ""
    except Exception as e:
        logger.error(f"Error loading SMED file: {e}")
        return ""
```

### 2. React WebTerminal
**파일**: `/home/aspuser/app/ofasp-refactor/src/components/WebTerminal.tsx`

#### CALL 명령어 처리
```typescript
const handleCommand = async (command: string): Promise<string> => {
    try {
        if (command.toUpperCase().startsWith('CALL ')) {
            const response = await fetch('http://localhost:8000/api/call', {
                method: 'POST',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify({ command })
            });
            
            const data = await response.json();
            
            if (data.success) {
                // SMED 맵이 있으면 화면 표시
                if (data.smed_content) {
                    displaySmedMap(data.smed_content);
                    
                    // Function Key 이벤트 핸들러 활성화
                    enableFunctionKeys();
                }
                
                return data.output;
            } else {
                return `Error: ${data.error}`;
            }
        }
        
        // 다른 명령어 처리
        return await processOtherCommands(command);
        
    } catch (error) {
        return `Connection error: ${error}`;
    }
};
```

#### SMED 맵 화면 표시
```typescript
const displaySmedMap = (smedContent: string) => {
    try {
        const lines = smedContent.split('\n');
        const screenBuffer = Array(24).fill(null).map(() => Array(80).fill(' '));
        
        lines.forEach(line => {
            if (line.trim().startsWith('ITEM ')) {
                const item = parseSmedItem(line);
                
                // 화면 버퍼에 텍스트 배치
                if (item.pos) {
                    const [row, col] = item.pos;
                    const text = item.prompt || '';
                    
                    for (let i = 0; i < text.length && col + i < 80; i++) {
                        screenBuffer[row - 1][col - 1 + i] = text[i];
                    }
                    
                    // 색상 정보 저장
                    if (item.color) {
                        setItemColor(row, col, text.length, item.color);
                    }
                }
            }
        });
        
        // 화면 업데이트
        updateTerminalDisplay(screenBuffer);
        
    } catch (error) {
        console.error('SMED map display error:', error);
    }
};
```

#### Function Key 처리
```typescript
const enableFunctionKeys = () => {
    const handleKeyDown = (event: KeyboardEvent) => {
        // F1-F12 키 감지
        if (event.key.startsWith('F') && event.key.length <= 3) {
            event.preventDefault();
            
            const functionKey = event.key;
            
            // Java 프로그램에 Function Key 이벤트 전송
            sendFunctionKeyToProgram(functionKey);
        }
    };
    
    document.addEventListener('keydown', handleKeyDown);
    
    // 정리 함수 등록
    return () => {
        document.removeEventListener('keydown', handleKeyDown);
    };
};

const sendFunctionKeyToProgram = async (functionKey: string) => {
    try {
        const response = await fetch('http://localhost:8000/api/function-key', {
            method: 'POST',
            headers: {'Content-Type': 'application/json'},
            body: JSON.stringify({ 
                key: functionKey,
                currentProgram: getCurrentProgramContext()
            })
        });
        
        const data = await response.json();
        
        if (data.success) {
            // 프로그램 응답 처리
            if (data.newScreen) {
                displaySmedMap(data.newScreen);
            }
            
            if (data.output) {
                appendToTerminal(data.output);
            }
        }
        
    } catch (error) {
        console.error('Function key error:', error);
    }
};
```

### 3. Java 프로그램 인터페이스
**파일**: `/home/aspuser/app/server/java_jars/` (JAR 내부)

#### 기본 프로그램 구조
```java
package com.openasp.testlib;

public class TestProgram {
    private String volume;
    private String library;
    private String currentMap = "MAINMENU";
    
    public static void main(String[] args) {
        TestProgram program = new TestProgram();
        program.parseArguments(args);
        program.execute();
    }
    
    public void execute() {
        // SMED 맵 정보 출력
        System.out.println("SMED_MAP:" + currentMap);
        
        // 프로그램 로직 실행
        displayMenu();
        
        // 입력 대기 (Function Key 처리)
        waitForInput();
    }
    
    public void displayMenu() {
        System.out.println("OpenASP メインメニュー");
        System.out.println("================================");
        System.out.println("1. 営業管理システム");
        System.out.println("2. 納品管理システム");  
        System.out.println("3. 在庫管理システム");
        System.out.println("4. ユーザー管理");
        System.out.println("5. システム情報");
        System.out.println("6. レポート出力");
        System.out.println("================================");
        System.out.println("選択してください (1-6): ");
    }
    
    public void handleFunctionKey(String key) {
        switch (key) {
            case "F1":
                showHelp();
                break;
            case "F3":
                exitProgram();
                break;
            case "F12":
                cancelOperation();
                break;
            case "Enter":
                processSelection();
                break;
        }
    }
}
```

### 4. SMED 파일 구조
**파일**: `/home/aspuser/app/volume/DISK01/TESTLIB/MAINMENU`

```
MAPNAME MAINMENU
  ITEM TITLE1 TYPE=T POS=(2,25) PROMPT="=================================" COLOR=#FF4444
  ITEM TITLE2 TYPE=T POS=(3,25) PROMPT="      OpenASP メインメニュー   " COLOR=#FFFF44
  ITEM TITLE3 TYPE=T POS=(4,25) PROMPT="=================================" COLOR=#FF4444
  ITEM MENU1 TYPE=T POS=(7,20) PROMPT="1. 営業管理システム " COLOR=#4ADE80
  ITEM MENU2 TYPE=T POS=(8,20) PROMPT="2. 納品管理システム " COLOR=#4ADE80
  ITEM MENU3 TYPE=T POS=(9,20) PROMPT="3. 在庫管理システム " COLOR=#4ADE80
  ITEM MENU4 TYPE=T POS=(10,20) PROMPT="4. ユーザー管理 " COLOR=#4ADE80
  ITEM MENU5 TYPE=T POS=(11,20) PROMPT="5. システム情報 " COLOR=#4ADE80
  ITEM MENU6 TYPE=T POS=(12,20) PROMPT="6. レポート出力" COLOR=#4ADE80
  ITEM SELECT_PROMPT TYPE=T POS=(15,20) PROMPT="選択してください (1-6): " COLOR=#FFFFFF
  ITEM HELP1 TYPE=T POS=(20,15) PROMPT="F1=ヘルプ　F3=終了　F12=キャンセル  Enter=実行" COLOR=#888888
  ITEM HELP2 TYPE=T POS=(22,20) PROMPT="ログイン名: " COLOR=#888888
```

## 설정 파일

### 1. SMED-PGM 매핑
**파일**: `/home/aspuser/app/src/smed_pgm.json`

```json
{
  "maps": {
    "MAINMENU": {
      "pgm_type": "JAVA",
      "pgm_class": "com.openasp.testlib.TestProgram",
      "description": "Main menu program",
      "function_keys": {
        "F1": "HELP",
        "F3": "EXIT", 
        "F12": "CANCEL",
        "Enter": "SELECT"
      }
    },
    "EIGYO001": {
      "pgm_type": "JAVA",
      "pgm_class": "com.openasp.eigyo.EIGYO001",
      "description": "Sales management",
      "function_keys": {
        "F1": "HELP",
        "F3": "EXIT",
        "F5": "REFRESH",
        "F12": "CANCEL"
      }
    }
  }
}
```

### 2. Catalog.json 구조
**파일**: `/home/aspuser/app/config/catalog.json`

```json
{
  "DISK01": {
    "TESTLIB": {
      "TestProgram": {
        "TYPE": "PGM",
        "PGMTYPE": "JAVA",
        "PGMNAME": "TestProgram",
        "DESCRIPTION": "Sample ASP Java Program for CALL testing",
        "JARFILE": "TestProgram.jar",
        "MAP": "MAINMENU"
      },
      "MAINMENU": {
        "TYPE": "MAP",
        "MAPTYPE": "SMED",
        "MAPFILE": "MAINMENU",
        "DESCRIPTION": "SMED map: MAINMENU",
        "ROWS": 24,
        "COLS": 80
      }
    }
  }
}
```

## 사용 방법

### 1. 개발자 가이드

#### 새로운 SMED 프로그램 추가
```bash
# 1. Java 프로그램 작성
cat > NewProgram.java << 'EOF'
package com.openasp.testlib;

public class NewProgram {
    public static void main(String[] args) {
        System.out.println("SMED_MAP:NEWMAP");
        System.out.println("New Program Output");
    }
}
EOF

# 2. 컴파일 및 JAR 추가
javac -cp ofasp.jar NewProgram.java
jar uf ofasp.jar com/openasp/testlib/NewProgram.class

# 3. SMED 맵 파일 생성 (SJIS)
cat > /home/aspuser/app/volume/DISK01/TESTLIB/NEWMAP << 'EOF'
MAPNAME NEWMAP
  ITEM TITLE TYPE=T POS=(1,1) PROMPT="New Program Screen" COLOR=#FFFF00
EOF

# 4. Catalog.json 업데이트
# 새 프로그램과 맵 정보 추가
```

#### API 확장
```python
@app.route('/api/smed/interactive/<path:program_id>', methods=['POST'])
def handle_interactive_smed(program_id):
    """인터랙티브 SMED 세션 처리"""
    try:
        data = request.get_json()
        user_input = data.get('input', '')
        function_key = data.get('function_key', '')
        
        # 프로그램 상태 관리
        session = get_or_create_program_session(program_id)
        
        # 사용자 입력 처리
        response = session.process_input(user_input, function_key)
        
        return jsonify({
            'success': True,
            'output': response.get('output', ''),
            'new_map': response.get('map_change', ''),
            'exit': response.get('exit', False)
        })
        
    except Exception as e:
        return jsonify({'success': False, 'error': str(e)}), 500
```

### 2. 최종 사용자 가이드

#### WebTerminal 사용법
1. **프로그램 실행**
   ```
   CALL PGM-TestProgram.TESTLIB,VOL-DISK01
   ```

2. **SMED 화면 조작**
   - **F1**: 도움말 표시
   - **F3**: 프로그램 종료
   - **F12**: 현재 작업 취소
   - **Enter**: 선택 실행
   - **숫자 키**: 메뉴 선택

3. **한국어/일본어 입력**
   - SMED 맵에서 한국어/일본어 정상 표시
   - 입력 필드에서 멀티바이트 문자 지원

## 테스트 방법

### 1. 통합 테스트
```bash
# 1. API 서버 시작
cd /home/aspuser/app/server
python api_server.py &

# 2. React 앱 시작  
cd /home/aspuser/app/ofasp-refactor
npm start &

# 3. WebTerminal 접속
# http://localhost:3000

# 4. CALL 명령어 테스트
CALL PGM-TestProgram.TESTLIB,VOL-DISK01
```

### 2. SMED 맵 테스트
```bash
# SMED 파일 직접 로드 테스트
curl "http://localhost:8000/api/smed/content/DISK01/TESTLIB/MAINMENU"

# Java 프로그램 실행 테스트
curl -X POST "http://localhost:8000/api/call" \
  -H "Content-Type: application/json" \
  -d '{"command":"CALL PGM-TestProgram.TESTLIB,VOL-DISK01"}'
```

### 3. Function Key 테스트
```bash
# Function Key 이벤트 테스트
curl -X POST "http://localhost:8000/api/function-key" \
  -H "Content-Type: application/json" \
  -d '{"key":"F1","currentProgram":"TestProgram"}'
```

## 성능 최적화

### 1. SMED 맵 캐싱
```python
from functools import lru_cache
import time

@lru_cache(maxsize=50)
def cached_load_smed_file(map_name, volume, library, cache_time=None):
    """SMED 파일 캐싱 로드"""
    file_path = f"/home/aspuser/app/volume/{volume}/{library}/{map_name}"
    
    with open(file_path, 'r', encoding='shift_jis', errors='replace') as f:
        return f.read()

# 캐시 무효화
def invalidate_smed_cache():
    cached_load_smed_file.cache_clear()
```

### 2. 세션 관리
```python
import threading
from datetime import datetime, timedelta

class ProgramSession:
    def __init__(self, program_id):
        self.program_id = program_id
        self.created_at = datetime.now()
        self.last_activity = datetime.now()
        self.state = {}
        
    def is_expired(self, timeout_minutes=30):
        return datetime.now() - self.last_activity > timedelta(minutes=timeout_minutes)

# 글로벌 세션 관리자
sessions = {}
session_lock = threading.Lock()

def cleanup_expired_sessions():
    """만료된 세션 정리"""
    with session_lock:
        expired = [sid for sid, session in sessions.items() if session.is_expired()]
        for sid in expired:
            del sessions[sid]
```

### 3. 비동기 처리
```typescript
// React에서 비동기 SMED 로딩
const useAsyncSmed = (mapName: string) => {
    const [smedContent, setSmedContent] = useState<string>('');
    const [loading, setLoading] = useState<boolean>(false);
    
    useEffect(() => {
        if (!mapName) return;
        
        setLoading(true);
        
        const loadSmed = async () => {
            try {
                const response = await fetch(`/api/smed/content/${mapName}`);
                const content = await response.text();
                setSmedContent(content);
            } catch (error) {
                console.error('SMED load error:', error);
            } finally {
                setLoading(false);
            }
        };
        
        loadSmed();
    }, [mapName]);
    
    return { smedContent, loading };
};
```

## 문제 해결

### 1. 일반적인 문제들

#### SMED 맵이 표시되지 않는 경우
**원인**: 
- 파일 경로 오류
- SJIS 인코딩 문제  
- Java 프로그램 오류

**해결법**:
```bash
# 파일 존재 확인
ls -la /home/aspuser/app/volume/DISK01/TESTLIB/

# 인코딩 확인
file -i /home/aspuser/app/volume/DISK01/TESTLIB/MAINMENU

# Java 프로그램 직접 실행
java -cp /home/aspuser/app/server/java_jars/ofasp.jar com.openasp.testlib.TestProgram
```

#### Function Key가 작동하지 않는 경우
**원인**:
- 이벤트 리스너 미등록
- 프로그램 세션 만료
- API 연결 오류

**해결법**:
```javascript
// 브라우저 개발자 도구에서 확인
console.log('Function key listeners:', document.eventListeners);

// 세션 상태 확인
fetch('/api/session/status').then(r => r.json()).then(console.log);
```

#### 한국어/일본어 깨짐 현상
**원인**: SJIS 인코딩 처리 오류

**해결법**:
```python
# api_server.py에서 인코딩 강제 설정
with open(file_path, 'r', encoding='shift_jis', errors='replace') as f:
    content = f.read()
```

### 2. 로그 분석
```bash
# API 서버 로그 확인
tail -f /tmp/api_server_new.log

# 특정 에러 검색
grep "ERROR\|Exception" /tmp/api_server_new.log

# SMED 관련 로그만 확인  
grep "SMED" /tmp/api_server_new.log
```

## 보안 고려사항

### 1. 입력 검증
```python
import re

def validate_call_command(command):
    """CALL 명령어 검증"""
    pattern = r'^CALL PGM-[A-Za-z0-9_]+\.[A-Za-z0-9_]+,VOL-[A-Za-z0-9_]+$'
    
    if not re.match(pattern, command):
        raise ValueError("Invalid CALL command format")
        
    # 허용된 프로그램만 실행
    allowed_programs = load_allowed_programs()
    program_name = extract_program_name(command)
    
    if program_name not in allowed_programs:
        raise PermissionError(f"Program {program_name} not allowed")
```

### 2. 세션 보안
```python
import secrets
import hashlib

def create_secure_session(user_id):
    """보안 세션 생성"""
    session_id = secrets.token_urlsafe(32)
    session_token = hashlib.sha256(f"{session_id}{user_id}".encode()).hexdigest()
    
    return {
        'session_id': session_id,
        'token': session_token,
        'created_at': datetime.now(),
        'user_id': user_id
    }
```

## 확장 계획

### 1. 단기 계획
- 더 많은 Function Key 지원 (Shift+F1, Ctrl+F1 등)
- SMED 맵 편집기와 실시간 연동
- 사용자별 세션 분리

### 2. 장기 계획  
- WebSocket 기반 실시간 통신
- 클러스터 환경 지원
- SMED 맵 버전 관리
- 사용자 권한 관리 시스템

## 관련 파일
- `/home/aspuser/app/server/api_server.py` - Flask API 서버
- `/home/aspuser/app/ofasp-refactor/src/components/WebTerminal.tsx` - React Terminal
- `/home/aspuser/app/server/java_jars/ofasp.jar` - Java 프로그램들  
- `/home/aspuser/app/volume/DISK01/TESTLIB/MAINMENU` - SMED 맵 파일
- `/home/aspuser/app/src/smed_pgm.json` - SMED-프로그램 매핑
- `/home/aspuser/app/config/catalog.json` - 리소스 카탈로그

## 참고 자료
- [ASP System Commands](https://github.com/jeiths2202/ofasp-react/docs/)
- [SMED File Format Specification](./SMED_FILE_FORMAT.md)
- [Java Program Development Guide](./JAVA_PROGRAM_GUIDE.md)