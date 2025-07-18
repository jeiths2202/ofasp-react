#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Conversion constants following CODING_RULES.md
All hardcoded values extracted to constants
"""

from typing import Dict, Set, List


class ConversionConstants:
    """Constants for EBCDIC/ASCII conversion"""
    
    # SOSI (Shift-Out/Shift-In) codes
    SOSI_SO = 0x0E  # Shift Out - start double-byte mode
    SOSI_SI = 0x0F  # Shift In - return to single-byte mode
    
    # Alternative SOSI codes
    SOSI_ALTERNATE_SO = 0x1E
    SOSI_ALTERNATE_SI = 0x1F
    
    # Default values
    DEFAULT_SPACE_CHAR = 0x20
    DEFAULT_QUESTION_MARK = 0x3F
    EBCDIC_SPACE_CHAR = 0x40
    
    # Japanese encoding constants
    SHIFT_JIS_DOUBLE_BYTE_MIN = 0x8140
    SHIFT_JIS_FULL_WIDTH_SPACE = 0x8140
    
    # Array size constants
    SINGLE_BYTE_ARRAY_SIZE = 256
    DOUBLE_BYTE_ARRAY_SIZE = 65536
    
    # Conversion directions
    EBCDIC_TO_ASCII = 'EBCDIC_TO_ASCII'
    ASCII_TO_EBCDIC = 'ASCII_TO_EBCDIC'
    
    # Supported encodings
    SUPPORTED_ENCODINGS: Set[str] = {'US', 'JP', 'JAK', 'KEIS', 'KR'}
    
    # Default encoding mappings
    ENCODING_FALLBACK_MAP: Dict[str, str] = {
        'KR': 'US',  # Korean uses US codepage as fallback
    }
    
    # SOSI handling modes
    SOSI_HANDLING_MODES: Set[str] = {'remove', 'keep', 'space'}
    
    # Error handling modes
    ERROR_HANDLING_MODES: Set[str] = {'strict', 'replace', 'ignore'}
    
    # File encoding settings
    FILE_ENCODING_MAP: Dict[str, str] = {
        'JP': 'shift_jis',
        'US': 'utf-8',
        'JAK': 'utf-8',
        'KEIS': 'utf-8',
        'KR': 'utf-8'
    }
    
    # Validation constants
    MAX_HEX_STRING_LENGTH = 2097152  # 1MB in hex chars
    MIN_RECORD_LENGTH = 1
    MAX_RECORD_LENGTH = 32767
    
    # API constants
    API_VERSION = 'v1'
    API_TIMEOUT_SECONDS = 300
    
    # HTTP status codes
    HTTP_OK = 200
    HTTP_BAD_REQUEST = 400
    HTTP_UNAUTHORIZED = 401
    HTTP_NOT_FOUND = 404
    HTTP_INTERNAL_SERVER_ERROR = 500
    
    # Content types
    CONTENT_TYPE_JSON = 'application/json'
    CONTENT_TYPE_TEXT = 'text/plain'
    
    # Cache constants
    CACHE_TTL_SECONDS = 3600
    MAX_CACHE_SIZE = 100


class ValidationRules:
    """Validation rules for conversion parameters"""
    
    @staticmethod
    def is_valid_encoding(encoding: str) -> bool:
        """Check if encoding is supported"""
        return encoding in ConversionConstants.SUPPORTED_ENCODINGS
    
    @staticmethod
    def is_valid_hex_string(hex_string: str) -> bool:
        """Validate hex string format"""
        if not hex_string:
            return False
        
        # Remove spaces and check length
        clean_hex = hex_string.replace(' ', '').replace('\n', '')
        if len(clean_hex) > ConversionConstants.MAX_HEX_STRING_LENGTH:
            return False
        
        # Check if all characters are valid hex
        try:
            int(clean_hex, 16)
            return True
        except ValueError:
            return False
    
    @staticmethod
    def is_valid_record_length(rlen: int) -> bool:
        """Validate record length"""
        return (ConversionConstants.MIN_RECORD_LENGTH <= rlen <= 
                ConversionConstants.MAX_RECORD_LENGTH)
    
    @staticmethod
    def is_valid_sosi_handling(handling: str) -> bool:
        """Check if SOSI handling mode is valid"""
        return handling in ConversionConstants.SOSI_HANDLING_MODES
    
    @staticmethod
    def is_valid_error_handling(handling: str) -> bool:
        """Check if error handling mode is valid"""
        return handling in ConversionConstants.ERROR_HANDLING_MODES


# Export constants for easy import
__all__ = ['ConversionConstants', 'ValidationRules']