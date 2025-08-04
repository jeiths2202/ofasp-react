#!/usr/bin/env python3
"""
Test script for Position-based SMED Rendering API
테스트용 스크립트 - Position-based SMED 렌더링 API
"""

import requests
import json
import sys

# API 서버 URL
BASE_URL = "http://localhost:8000"

def test_position_api():
    """Position-based SMED API 테스트"""
    
    print("=== Position-based SMED API 테스트 시작 ===\n")
    
    # 1. 빈 맵 목록 확인
    print("1. 빈 맵 목록 확인")
    try:
        response = requests.get(f"{BASE_URL}/api/smed/position-render")
        print(f"Status: {response.status_code}")
        print(f"Response: {json.dumps(response.json(), indent=2, ensure_ascii=False)}")
        print()
    except Exception as e:
        print(f"Error: {e}\n")
    
    # 2. 새로운 position map 생성
    print("2. 새로운 position map 생성 (EMPLOYEE_INFO)")
    sample_map = [
        {"row": 0, "col": 0, "length": 20},   # 제목
        {"row": 2, "col": 5, "length": 10},   # 사번
        {"row": 3, "col": 5, "length": 15},   # 성명
        {"row": 4, "col": 5, "length": 8},    # 입사일
        {"row": 6, "col": 10, "length": 30},  # 부서명
    ]
    
    try:
        response = requests.put(
            f"{BASE_URL}/api/smed/position-render/EMPLOYEE_INFO",
            json={"map": sample_map}
        )
        print(f"Status: {response.status_code}")
        print(f"Response: {json.dumps(response.json(), indent=2, ensure_ascii=False)}")
        print()
    except Exception as e:
        print(f"Error: {e}\n")
    
    # 3. 생성된 맵 조회
    print("3. 생성된 맵 조회")
    try:
        response = requests.get(f"{BASE_URL}/api/smed/position-render/EMPLOYEE_INFO")
        print(f"Status: {response.status_code}")
        print(f"Response: {json.dumps(response.json(), indent=2, ensure_ascii=False)}")
        print()
    except Exception as e:
        print(f"Error: {e}\n")
    
    # 4. 데이터 렌더링 테스트
    print("4. 데이터 렌더링 테스트")
    sample_data = [
        "사원 정보 조회",           # 제목
        "E001234",              # 사번
        "김철수",                # 성명  
        "20200315",             # 입사일
        "정보시스템팀"            # 부서명
    ]
    
    try:
        response = requests.post(
            f"{BASE_URL}/api/smed/position-render/EMPLOYEE_INFO/data",
            json={"data": sample_data}
        )
        print(f"Status: {response.status_code}")
        result = response.json()
        
        if result.get('success'):
            print("렌더링 성공!")
            print(f"맵 이름: {result['map_name']}")
            print(f"필드 수: {len(result['fields'])}")
            print("\n렌더링된 그리드:")
            for i, line in enumerate(result['grid']):
                if line.strip():  # 비어있지 않은 라인만 출력
                    print(f"{i:2d}: {repr(line)}")
        else:
            print(f"Response: {json.dumps(result, indent=2, ensure_ascii=False)}")
        print()
    except Exception as e:
        print(f"Error: {e}\n")
    
    # 5. 데이터 업데이트 테스트 (WebSocket 브로드캐스트 포함)
    print("5. 데이터 업데이트 테스트")
    updated_data = [
        "사원 정보 수정됨",         # 제목
        "E001234",              # 사번
        "김철수(수정)",           # 성명  
        "20200315",             # 입사일
        "개발팀"                 # 부서명 (변경)
    ]
    
    try:
        response = requests.put(
            f"{BASE_URL}/api/smed/position-render/EMPLOYEE_INFO/data",
            json={
                "data": updated_data,
                "terminal_id": "test_terminal"
            }
        )
        print(f"Status: {response.status_code}")
        result = response.json()
        
        if result.get('success'):
            print("업데이트 및 브로드캐스트 성공!")
            print(f"메시지: {result['message']}")
        else:
            print(f"Response: {json.dumps(result, indent=2, ensure_ascii=False)}")
        print()
    except Exception as e:
        print(f"Error: {e}\n")
    
    # 6. 업데이트된 맵 목록 확인
    print("6. 업데이트된 맵 목록 확인")
    try:
        response = requests.get(f"{BASE_URL}/api/smed/position-render")
        print(f"Status: {response.status_code}")
        print(f"Response: {json.dumps(response.json(), indent=2, ensure_ascii=False)}")
        print()
    except Exception as e:
        print(f"Error: {e}\n")
    
    # 7. 에러 케이스 테스트 - 잘못된 데이터
    print("7. 에러 케이스 테스트 - 데이터 길이 불일치")
    try:
        response = requests.post(
            f"{BASE_URL}/api/smed/position-render/EMPLOYEE_INFO/data",
            json={"data": ["데이터", "부족"]}  # 5개 필요하지만 2개만 제공
        )
        print(f"Status: {response.status_code}")
        print(f"Response: {json.dumps(response.json(), indent=2, ensure_ascii=False)}")
        print()
    except Exception as e:
        print(f"Error: {e}\n")
    
    # 8. 에러 케이스 테스트 - 존재하지 않는 맵
    print("8. 에러 케이스 테스트 - 존재하지 않는 맵")
    try:
        response = requests.get(f"{BASE_URL}/api/smed/position-render/NON_EXISTENT_MAP")
        print(f"Status: {response.status_code}")
        print(f"Response: {json.dumps(response.json(), indent=2, ensure_ascii=False)}")
        print()
    except Exception as e:
        print(f"Error: {e}\n")
    
    print("=== Position-based SMED API 테스트 완료 ===")

def test_validation():
    """유효성 검증 테스트"""
    
    print("\n=== 유효성 검증 테스트 시작 ===\n")
    
    # 1. 잘못된 맵 구조 - row 범위 초과
    print("1. 잘못된 맵 구조 테스트 - row 범위 초과")
    invalid_map = [
        {"row": 25, "col": 0, "length": 10}  # row는 0-23이어야 함
    ]
    
    try:
        response = requests.put(
            f"{BASE_URL}/api/smed/position-render/INVALID_MAP",
            json={"map": invalid_map}
        )
        print(f"Status: {response.status_code}")
        print(f"Response: {json.dumps(response.json(), indent=2, ensure_ascii=False)}")
        print()
    except Exception as e:
        print(f"Error: {e}\n")
    
    # 2. 잘못된 맵 구조 - 그리드 폭 초과
    print("2. 잘못된 맵 구조 테스트 - 그리드 폭 초과")
    invalid_map2 = [
        {"row": 0, "col": 75, "length": 10}  # col 75 + length 10 = 85 > 80
    ]
    
    try:
        response = requests.put(
            f"{BASE_URL}/api/smed/position-render/INVALID_MAP2",
            json={"map": invalid_map2}
        )
        print(f"Status: {response.status_code}")
        print(f"Response: {json.dumps(response.json(), indent=2, ensure_ascii=False)}")
        print()
    except Exception as e:
        print(f"Error: {e}\n")
    
    # 3. 잘못된 데이터 타입
    print("3. 잘못된 데이터 타입 테스트")
    try:
        response = requests.put(
            f"{BASE_URL}/api/smed/position-render/TYPE_TEST",
            json={"map": "not_a_list"}  # list가 아닌 string
        )
        print(f"Status: {response.status_code}")
        print(f"Response: {json.dumps(response.json(), indent=2, ensure_ascii=False)}")
        print()
    except Exception as e:
        print(f"Error: {e}\n")
    
    print("=== 유효성 검증 테스트 완료 ===")

if __name__ == "__main__":
    print("Position-based SMED API 테스트 시작...")
    print("API 서버가 http://localhost:8000 에서 실행 중인지 확인하세요.\n")
    
    try:
        # 서버 연결 확인
        response = requests.get(f"{BASE_URL}/api/health", timeout=5)
        if response.status_code == 200:
            print("✓ API 서버 연결 확인됨\n")
            test_position_api()
            test_validation()
        else:
            print(f"× API 서버 응답 오류: {response.status_code}")
            sys.exit(1)
    except requests.exceptions.RequestException as e:
        print(f"× API 서버에 연결할 수 없습니다: {e}")
        print("API 서버를 먼저 시작해주세요: python api_server.py")
        sys.exit(1)