#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Test SOSI handling parameter
"""

import requests
import json

def test_sosi_handling():
    """Test different SOSI handling modes"""
    
    base_url = "http://localhost:8001"
    
    # Test data with SOSI codes
    test_data = {
        "input_data": "4040404040404040404040C4C9E2D7D3C1E8407D0E43AA435843C543584A7E49BA404040400F7D4B40404040404040404040404040404040404040404040404040404040404040404040404040404040",
        "encoding": "JP",
        "sosi_flag": True,
        "out_sosi_flag": False,
        "rlen": 80
    }
    
    print("Testing SOSI handling modes...")
    print("=" * 60)
    
    # Test 1: Remove mode (default)
    print("\n1. Testing 'remove' mode:")
    test_data["sosi_handling"] = "remove"
    response = requests.post(f"{base_url}/api/v1/convert/ebcdic-to-ascii", json=test_data)
    if response.status_code == 200:
        result = response.json()
        if result['success']:
            output = result['data']['output']
            print(f"Output: '{output}'")
            print(f"Length: {len(output)}")
            print(f"Hex: {output.encode().hex()}")
    
    # Test 2: Space mode
    print("\n2. Testing 'space' mode:")
    test_data["sosi_handling"] = "space"
    response = requests.post(f"{base_url}/api/v1/convert/ebcdic-to-ascii", json=test_data)
    if response.status_code == 200:
        result = response.json()
        if result['success']:
            output = result['data']['output']
            print(f"Output: '{output}'")
            print(f"Length: {len(output)}")
            print(f"Hex: {output.encode().hex()}")
    
    # Test 3: Keep mode
    print("\n3. Testing 'keep' mode:")
    test_data["sosi_handling"] = "keep"
    response = requests.post(f"{base_url}/api/v1/convert/ebcdic-to-ascii", json=test_data)
    if response.status_code == 200:
        result = response.json()
        if result['success']:
            output = result['data']['output']
            print(f"Output: '{output}'")
            print(f"Length: {len(output)}")
            print(f"Hex: {output.encode().hex()}")
            # Show SOSI codes
            for i, char in enumerate(output):
                if ord(char) == 0x0E:
                    print(f"  SO at position {i}")
                elif ord(char) == 0x0F:
                    print(f"  SI at position {i}")

if __name__ == '__main__':
    test_sosi_handling()