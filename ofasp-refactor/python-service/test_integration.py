#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Integration test for Python conversion service
Tests the actual API endpoints and conversion functionality
"""

import requests
import json
import sys
from pathlib import Path

# Add project root to Python path
sys.path.insert(0, str(Path(__file__).parent))

from config.config import config


def test_python_service():
    """Test Python conversion service integration"""
    
    base_url = f"http://localhost:{config.flask_config['port']}"
    
    print("=" * 60)
    print("Python Conversion Service Integration Test")
    print("=" * 60)
    
    # Test 1: Health check
    print("\n1. Testing health check...")
    try:
        response = requests.get(f"{base_url}/health", timeout=5)
        if response.status_code == 200:
            data = response.json()
            if data.get('success'):
                print("✅ Health check passed")
            else:
                print("❌ Health check failed: response not successful")
                return False
        else:
            print(f"❌ Health check failed: HTTP {response.status_code}")
            return False
    except Exception as e:
        print(f"❌ Health check failed: {e}")
        return False
    
    # Test 2: Service info
    print("\n2. Testing service info...")
    try:
        response = requests.get(f"{base_url}/api/v1/info", timeout=5)
        if response.status_code == 200:
            data = response.json()
            if data.get('success'):
                info = data['data']
                print(f"✅ Service info retrieved")
                print(f"   Version: {info.get('version')}")
                print(f"   Supported encodings: {info.get('supported_encodings')}")
                print(f"   Default encoding: {info.get('default_encoding')}")
                print(f"   Max input size: {info.get('max_input_size')}")
            else:
                print("❌ Service info failed: response not successful")
                return False
        else:
            print(f"❌ Service info failed: HTTP {response.status_code}")
            return False
    except Exception as e:
        print(f"❌ Service info failed: {e}")
        return False
    
    # Test 3: EBCDIC to ASCII conversion
    print("\n3. Testing EBCDIC to ASCII conversion...")
    test_data = {
        "input_data": "C4C9E2D7D3C1E8",  # "DISPLAY" in EBCDIC
        "encoding": "US",
        "sosi_flag": False,
        "out_sosi_flag": False,
        "rlen": 80
    }
    
    try:
        response = requests.post(
            f"{base_url}/api/v1/convert/ebcdic-to-ascii",
            json=test_data,
            timeout=10
        )
        if response.status_code == 200:
            data = response.json()
            if data.get('success'):
                output = data['data']['output']
                print(f"✅ EBCDIC to ASCII conversion successful")
                print(f"   Input: {test_data['input_data']}")
                print(f"   Output: '{output}'")
                if "DISPLAY" in output:
                    print("✅ Output contains expected 'DISPLAY' text")
                else:
                    print("❌ Output does not contain expected 'DISPLAY' text")
                    return False
            else:
                print(f"❌ EBCDIC to ASCII conversion failed: {data.get('error')}")
                return False
        else:
            print(f"❌ EBCDIC to ASCII conversion failed: HTTP {response.status_code}")
            return False
    except Exception as e:
        print(f"❌ EBCDIC to ASCII conversion failed: {e}")
        return False
    
    # Test 4: EBCDIC to ASCII with SOSI (Japanese)
    print("\n4. Testing EBCDIC to ASCII conversion with SOSI...")
    test_data_sosi = {
        "input_data": "4040404040404040404040C4C9E2D7D3C1E8407D0E43AA435843C543584A7E49BA404040400F7D4B40404040404040404040404040404040404040404040404040404040404040404040404040404040",
        "encoding": "JP",
        "sosi_flag": True,
        "out_sosi_flag": False,
        "rlen": 80
    }
    
    try:
        response = requests.post(
            f"{base_url}/api/v1/convert/ebcdic-to-ascii",
            json=test_data_sosi,
            timeout=10
        )
        if response.status_code == 200:
            data = response.json()
            if data.get('success'):
                output = data['data']['output']
                print(f"✅ EBCDIC to ASCII with SOSI conversion successful")
                print(f"   Output length: {len(output)}")
                print(f"   Output: '{output}'")
                if "DISPLAY" in output:
                    print("✅ Output contains expected 'DISPLAY' text")
                else:
                    print("❌ Output does not contain expected 'DISPLAY' text")
                    return False
            else:
                print(f"❌ EBCDIC to ASCII with SOSI conversion failed: {data.get('error')}")
                return False
        else:
            print(f"❌ EBCDIC to ASCII with SOSI conversion failed: HTTP {response.status_code}")
            return False
    except Exception as e:
        print(f"❌ EBCDIC to ASCII with SOSI conversion failed: {e}")
        return False
    
    # Test 5: ASCII to EBCDIC conversion
    print("\n5. Testing ASCII to EBCDIC conversion...")
    test_data_reverse = {
        "input_data": "DISPLAY",
        "encoding": "US",
        "sosi_flag": False,
        "out_sosi_flag": False,
        "rlen": 80
    }
    
    try:
        response = requests.post(
            f"{base_url}/api/v1/convert/ascii-to-ebcdic",
            json=test_data_reverse,
            timeout=10
        )
        if response.status_code == 200:
            data = response.json()
            if data.get('success'):
                output = data['data']['output']
                print(f"✅ ASCII to EBCDIC conversion successful")
                print(f"   Input: '{test_data_reverse['input_data']}'")
                print(f"   Output: {output}")
                if output == "C4C9E2D7D3C1E8":
                    print("✅ Output matches expected EBCDIC hex")
                else:
                    print("❌ Output does not match expected EBCDIC hex")
                    return False
            else:
                print(f"❌ ASCII to EBCDIC conversion failed: {data.get('error')}")
                return False
        else:
            print(f"❌ ASCII to EBCDIC conversion failed: HTTP {response.status_code}")
            return False
    except Exception as e:
        print(f"❌ ASCII to EBCDIC conversion failed: {e}")
        return False
    
    print("\n" + "=" * 60)
    print("✅ All integration tests passed!")
    print("✅ Python conversion service is working correctly")
    print("✅ Ready for use by OpenASP AX web interface")
    print("=" * 60)
    
    return True


if __name__ == '__main__':
    success = test_python_service()
    sys.exit(0 if success else 1)