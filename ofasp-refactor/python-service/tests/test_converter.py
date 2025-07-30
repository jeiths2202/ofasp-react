#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
TEST FILE - Separate from production code
Tests for EBCDIC converter following CODING_RULES.md
"""

import sys
import os
from pathlib import Path
import unittest

# Add project root to Python path
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.converters.ebcdic_converter import converter
from src.constants.conversion import ConversionConstants


class TestEbcdicConverter(unittest.TestCase):
    """Test cases for EBCDIC converter"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.converter = converter
    
    def test_basic_ebcdic_to_ascii(self):
        """Test basic EBCDIC to ASCII conversion"""
        # Test "DISPLAY" in EBCDIC
        test_ebcdic = "C4C9E2D7D3C1E8"
        result = self.converter.convert_ebcdic_to_ascii(
            input_data=test_ebcdic,
            encoding='US',
            sosi_flag=False,
            rlen=80
        )
        self.assertIn("DISPLAY", result)
    
    def test_sosi_conversion(self):
        """Test SOSI conversion"""
        # Test with SOSI codes
        test_ebcdic_sosi = "C4C9E2D7D3C1E8400E43AA0F40"
        result = self.converter.convert_ebcdic_to_ascii(
            input_data=test_ebcdic_sosi,
            encoding='JP',
            sosi_flag=True,
            rlen=80
        )
        self.assertIn("DISPLAY", result)
    
    def test_ascii_to_ebcdic(self):
        """Test ASCII to EBCDIC conversion"""
        test_ascii = "DISPLAY"
        result = self.converter.convert_ascii_to_ebcdic(
            input_data=test_ascii,
            encoding='US',
            sosi_flag=False,
            rlen=80
        )
        self.assertEqual(result, "C4C9E2D7D3C1E8")
    
    def test_invalid_encoding(self):
        """Test invalid encoding handling"""
        with self.assertRaises(ValueError):
            self.converter.convert_ebcdic_to_ascii(
                input_data="C4C9E2D7D3C1E8",
                encoding='INVALID',
                sosi_flag=False,
                rlen=80
            )
    
    def test_invalid_record_length(self):
        """Test invalid record length handling"""
        with self.assertRaises(ValueError):
            self.converter.convert_ebcdic_to_ascii(
                input_data="C4C9E2D7D3C1E8",
                encoding='US',
                sosi_flag=False,
                rlen=-1
            )
    
    def test_invalid_hex_string(self):
        """Test invalid hex string handling"""
        with self.assertRaises(ValueError):
            self.converter.convert_ebcdic_to_ascii(
                input_data="INVALID_HEX",
                encoding='US',
                sosi_flag=False,
                rlen=80
            )


class TestAPI(unittest.TestCase):
    """Test cases for API"""
    
    def setUp(self):
        """Set up test fixtures"""
        from src.api.app import api
        self.app = api.app
        self.client = self.app.test_client()
    
    def test_health_check(self):
        """Test health check endpoint"""
        response = self.client.get('/health')
        self.assertEqual(response.status_code, 200)
        data = response.get_json()
        self.assertTrue(data['success'])
    
    def test_service_info(self):
        """Test service info endpoint"""
        response = self.client.get('/api/v1/info')
        self.assertEqual(response.status_code, 200)
        data = response.get_json()
        self.assertTrue(data['success'])
        self.assertIn('supported_encodings', data['data'])
    
    def test_ebcdic_to_ascii_api(self):
        """Test EBCDIC to ASCII API endpoint"""
        test_data = {
            'input_data': 'C4C9E2D7D3C1E8',
            'encoding': 'US',
            'sosi_flag': False,
            'rlen': 80
        }
        response = self.client.post('/api/v1/convert/ebcdic-to-ascii', json=test_data)
        self.assertEqual(response.status_code, 200)
        data = response.get_json()
        self.assertTrue(data['success'])
        self.assertIn('DISPLAY', data['data']['output'])
    
    def test_ascii_to_ebcdic_api(self):
        """Test ASCII to EBCDIC API endpoint"""
        test_data = {
            'input_data': 'DISPLAY',
            'encoding': 'US',
            'sosi_flag': False,
            'rlen': 80
        }
        response = self.client.post('/api/v1/convert/ascii-to-ebcdic', json=test_data)
        self.assertEqual(response.status_code, 200)
        data = response.get_json()
        self.assertTrue(data['success'])
        self.assertEqual(data['data']['output'], 'C4C9E2D7D3C1E8')


if __name__ == '__main__':
    # Run tests
    unittest.main()