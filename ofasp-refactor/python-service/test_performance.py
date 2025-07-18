#!/usr/bin/env python3
"""
Performance test for batch vs line-by-line conversion
"""

import time
import requests
import json

# Test data - first few lines from sample.ebc
test_hex = "404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040"

def test_api_conversion(hex_data, description):
    """Test API conversion and measure time"""
    print(f"\n=== {description} ===")
    
    url = "http://localhost:8001/api/v1/convert/ebcdic-to-ascii"
    data = {
        "input_data": hex_data,
        "encoding": "JP",
        "sosi_flag": True,
        "out_sosi_flag": False,
        "rlen": 80,
        "sosi_handling": "space"
    }
    
    start_time = time.time()
    
    try:
        response = requests.post(url, json=data, headers={'Content-Type': 'application/json'})
        end_time = time.time()
        
        if response.status_code == 200:
            result = response.json()
            print(f"✓ Success: {result['data']['output_size']} characters converted")
            print(f"⏱ Time: {(end_time - start_time)*1000:.2f}ms")
            return result['data']['output']
        else:
            print(f"✗ Failed: {response.status_code} - {response.text}")
            return None
            
    except Exception as e:
        end_time = time.time()
        print(f"✗ Error: {str(e)}")
        print(f"⏱ Time: {(end_time - start_time)*1000:.2f}ms")
        return None

if __name__ == "__main__":
    print("Testing Python service performance...")
    
    # Test 1: Single line conversion (simulates current line-by-line approach)
    single_line = test_hex[:160]  # 80 bytes = 160 hex chars
    test_api_conversion(single_line, "Single Line Conversion (80 bytes)")
    
    # Test 2: Multi-line conversion (simulates batch approach)
    multi_line = test_hex * 10  # 10 lines worth of data
    test_api_conversion(multi_line, "Batch Conversion (800 bytes)")
    
    # Test 3: Health check
    print("\n=== Health Check ===")
    try:
        response = requests.get("http://localhost:8001/health")
        if response.status_code == 200:
            print("✓ Service is healthy")
        else:
            print(f"✗ Health check failed: {response.status_code}")
    except Exception as e:
        print(f"✗ Health check error: {str(e)}")
    
    print("\n=== Performance Test Complete ===")