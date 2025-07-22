# SJIS to Unicode / Unicode to SJIS 변환 기능

## 개요
OpenASP React 프로젝트의 핵심 기능 중 하나인 SJIS(Shift-JIS) 인코딩과 Unicode(UTF-8) 간의 양방향 변환 기능에 대한 상세 가이드입니다.

## 기술적 배경

### 문제 상황
- **서버 환경**: ASP 시스템 파일들이 SJIS 인코딩으로 저장됨
- **웹 환경**: React 애플리케이션은 UTF-8 인코딩 사용
- **일본어 텍스트**: 더블바이트 문자로 인한 표시 오류 (mojibake) 발생
- **예시**: "メインメニュー" → "繝｡繧､繝ｳ繝｡繝九Η繝ｼ" (잘못된 표시)

### 해결 방법
Python Flask API 서버에서 encoding 파라미터를 사용한 자동 변환:
```python
# SJIS 파일 읽기
with open(file_path, 'r', encoding='shift_jis', errors='replace') as f:
    content = f.read()

# SJIS 파일 쓰기  
with open(file_path, 'w', encoding='shift_jis', errors='replace') as f:
    f.write(content)
```

## 구현 위치

### 1. API 서버 (Flask)
**파일**: `/home/aspuser/app/server/api_server.py`

#### SMED 파일 읽기 (SJIS → UTF-8)
```python
@app.route('/api/smed/content/<path:smed_path>')
def get_smed_content_from_volume(smed_path):
    try:
        # SJIS 인코딩으로 파일 읽기
        with open(file_path, 'r', encoding='shift_jis', errors='replace') as f:
            content = f.read()
        
        # UTF-8로 자동 변환되어 반환
        return jsonify({
            'success': True,
            'content': content,
            'encoding': 'utf-8'
        })
    except Exception as e:
        return jsonify({'success': False, 'error': str(e)}), 500
```

#### SMED 파일 저장 (UTF-8 → SJIS)  
```python
@app.route('/api/smed/save', methods=['POST'])
def save_smed_file():
    try:
        data = request.get_json()
        filename = data.get('filename')  # 확장자 추가하지 않음
        content = data.get('content')
        
        # SJIS 인코딩으로 파일 저장
        with open(file_path, 'w', encoding='shift_jis', errors='replace') as f:
            f.write(content)
            
        return jsonify({'success': True})
    except Exception as e:
        return jsonify({'success': False, 'error': str(e)}), 500
```

### 2. React 프론트엔드
**파일**: `/home/aspuser/app/ofasp-refactor/src/components/ASPMapEditor.tsx`

#### SMED 파일 로딩
```typescript
const loadCatalogMap = async (map: any) => {
    try {
        const mapPath = `${map.volume}/${map.library}/${map.mapfile}`;
        const response = await fetch(`http://localhost:8000/api/smed/content/${mapPath}`);
        if (response.ok) {
            const content = await response.text(); // UTF-8로 자동 변환됨
            parseSmedContent(content);
        }
    } catch (error) {
        console.error('Error loading SMED:', error);
    }
};
```

#### SMED 파일 저장
```typescript
const performSave = async (filename: string, volume: string, library: string) => {
    try {
        const response = await fetch('http://localhost:8000/api/smed/save', {
            method: 'POST',
            headers: {'Content-Type': 'application/json'},
            body: JSON.stringify({
                filename,        // 확장자 없이 전송
                content: smedContent, // UTF-8 콘텐츠
                volume,
                library
            })
        });
    } catch (error) {
        console.error('Save error:', error);
    }
};
```

## 핵심 설정

### 1. 파일 확장자 관리
**중요**: SMED 파일 저장 시 `.smed` 확장자를 자동으로 추가하지 않음
```python
# ❌ 잘못된 방법
smed_filename = filename + '.smed'

# ✅ 올바른 방법  
smed_filename = filename  # 파일명 그대로 사용
```

### 2. 인코딩 에러 처리
```python
# 안전한 인코딩 변환
encoding='shift_jis', errors='replace'
```
- `errors='replace'`: 변환할 수 없는 문자를 안전하게 처리

### 3. Catalog.json 연동
```python
# catalog.json에 MAPFILE 등록 (확장자 없이)
catalog_entry = {
    "TYPE": "MAP",
    "MAPTYPE": "SMED", 
    "MAPFILE": filename,  # 확장자 없음
    "DESCRIPTION": f"SMED map: {filename}"
}
```

## 사용 방법

### 개발자 가이드

#### 1. 새로운 SJIS 파일 타입 추가
```python
@app.route('/api/new-file-type/<path:file_path>')
def handle_new_file_type(file_path):
    try:
        # SJIS 읽기
        with open(file_path, 'r', encoding='shift_jis', errors='replace') as f:
            content = f.read()
        
        # 처리 로직
        processed_content = process_content(content)
        
        return jsonify({'content': processed_content})
    except Exception as e:
        return jsonify({'error': str(e)}), 500
```

#### 2. 프론트엔드에서 SJIS 파일 처리
```typescript
const loadSjisFile = async (filePath: string) => {
    try {
        const response = await fetch(`http://localhost:8000/api/new-file-type/${filePath}`);
        const data = await response.json();
        
        // UTF-8로 변환된 콘텐츠 사용
        setContent(data.content);
    } catch (error) {
        console.error('SJIS file load error:', error);
    }
};
```

### 최종 사용자 가이드

#### 1. SMED Map Editor에서 일본어 파일 편집
1. **파일 로드**: "Catalog에서 로드" 버튼 사용
2. **편집**: 일본어 텍스트가 올바르게 표시됨
3. **저장**: "SMED 저장" 버튼으로 SJIS 형식으로 저장

#### 2. WebTerminal에서 SJIS 파일 확인
```bash
CALL PGM-TestProgram.TESTLIB,VOL-DISK01
```
- 서버의 SJIS 파일이 올바른 일본어로 표시됨

## 테스트 방법

### 1. API 테스트
```bash
# SJIS 파일 읽기 테스트
curl "http://localhost:8000/api/smed/content/DISK01/TESTLIB/MAINMENU"

# SJIS 파일 저장 테스트  
curl -X POST "http://localhost:8000/api/smed/save" \
  -H "Content-Type: application/json" \
  -d '{"filename":"TEST","content":"テスト","volume":"DISK01","library":"TESTLIB"}'
```

### 2. 파일 인코딩 확인
```bash
# 파일 인코딩 확인
file -i /home/aspuser/app/volume/DISK01/TESTLIB/MAINMENU

# SJIS 인코딩으로 저장되었는지 확인
hexdump -C /home/aspuser/app/volume/DISK01/TESTLIB/MAINMENU | head
```

### 3. 웹 브라우저 테스트
1. Map Editor 접속: `http://localhost:3000`
2. "Catalog에서 로드" → MAINMENU 선택
3. 일본어 텍스트 확인: "メインメニュー"
4. 텍스트 수정 후 저장
5. 다시 로드하여 변경사항 확인

## 문제 해결

### 1. 문자가 깨져 보이는 경우
**원인**: 잘못된 인코딩 설정
**해결**: API 서버에서 `encoding='shift_jis'` 확인

### 2. 저장된 파일을 다른 시스템에서 읽을 수 없는 경우
**원인**: 인코딩 불일치
**해결**: 저장 시 `encoding='shift_jis'` 사용 확인

### 3. 확장자 문제
**원인**: `.smed` 확장자 자동 추가
**해결**: 파일명 그대로 사용하도록 코드 수정

## 성능 고려사항

### 1. 메모리 사용량
- 큰 파일의 경우 스트리밍 처리 고려
- 배치 처리로 다중 파일 변환 최적화

### 2. 캐싱
```python
# 자주 사용되는 파일의 경우 캐싱 적용
from functools import lru_cache

@lru_cache(maxsize=100)
def load_cached_smed_file(file_path):
    with open(file_path, 'r', encoding='shift_jis') as f:
        return f.read()
```

## 보안 고려사항

### 1. 파일 경로 검증
```python
import os
from pathlib import Path

def validate_file_path(file_path):
    # 상위 디렉토리 접근 방지
    if '..' in file_path:
        raise ValueError("Invalid file path")
    
    # 허용된 디렉토리 내부인지 확인
    base_path = Path('/home/aspuser/app/volume')
    full_path = base_path / file_path
    
    if not str(full_path).startswith(str(base_path)):
        raise ValueError("Access denied")
```

### 2. 인코딩 에러 처리
```python
try:
    with open(file_path, 'r', encoding='shift_jis', errors='strict') as f:
        content = f.read()
except UnicodeDecodeError:
    # 대체 인코딩 시도
    with open(file_path, 'r', encoding='shift_jis', errors='replace') as f:
        content = f.read()
```

## 확장 가능성

### 1. 다른 인코딩 지원
- EUC-JP
- ISO-2022-JP  
- CP932

### 2. 자동 인코딩 감지
```python
import chardet

def detect_encoding(file_path):
    with open(file_path, 'rb') as f:
        raw_data = f.read()
    result = chardet.detect(raw_data)
    return result['encoding']
```

## 관련 파일
- `/home/aspuser/app/server/api_server.py` - Flask API 서버
- `/home/aspuser/app/ofasp-refactor/src/components/ASPMapEditor.tsx` - React Map Editor
- `/home/aspuser/app/config/catalog.json` - 리소스 카탈로그
- `/home/aspuser/app/volume/DISK01/TESTLIB/MAINMENU` - 예제 SJIS 파일

## 참고 자료
- [Shift JIS Wikipedia](https://en.wikipedia.org/wiki/Shift_JIS)
- [Python codecs documentation](https://docs.python.org/3/library/codecs.html)
- [Flask encoding best practices](https://flask.palletsprojects.com/en/2.0.x/)