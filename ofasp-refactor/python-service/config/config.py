#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Configuration module for Python conversion service
Follows CODING_RULES.md - NO HARDCODING policy
"""

import os
from pathlib import Path
from typing import Dict, Any, Optional


class Config:
    """Configuration class that loads settings from environment variables"""
    
    def __init__(self):
        """Initialize configuration with environment variables"""
        self._load_env_file()
        self._validate_required_vars()
    
    def _load_env_file(self):
        """Load environment variables from .env file if it exists"""
        env_file = Path(__file__).parent.parent / '.env'
        if env_file.exists():
            try:
                with open(env_file, 'r', encoding='utf-8') as f:
                    for line in f:
                        line = line.strip()
                        if line and not line.startswith('#') and '=' in line:
                            key, value = line.split('=', 1)
                            os.environ.setdefault(key.strip(), value.strip())
            except Exception as e:
                print(f"Warning: Could not load .env file: {e}")
    
    def _validate_required_vars(self):
        """Validate that required environment variables are set"""
        required_vars = [
            'CODEPAGE_BASE_PATH',
        ]
        
        missing_vars = []
        for var in required_vars:
            if not os.environ.get(var):
                missing_vars.append(var)
        
        if missing_vars:
            raise ValueError(f"Missing required environment variables: {', '.join(missing_vars)}")
    
    @property
    def flask_config(self) -> Dict[str, Any]:
        """Flask server configuration"""
        return {
            'port': int(os.environ.get('FLASK_PORT', '8001')),
            'host': os.environ.get('FLASK_HOST', '0.0.0.0'),
            'debug': os.environ.get('FLASK_DEBUG', 'false').lower() == 'true',
            'env': os.environ.get('FLASK_ENV', 'production')
        }
    
    @property
    def codepage_config(self) -> Dict[str, Any]:
        """Code page configuration"""
        base_path = os.environ.get('CODEPAGE_BASE_PATH', '../public/codepages')
        return {
            'base_path': Path(base_path).resolve(),
            'cache_size': int(os.environ.get('CODEPAGE_CACHE_SIZE', '10')),
            'files': {
                'EBCDIC_TO_ASCII': {
                    'US': 'EBCASCUS.txt',
                    'JP': 'EBCASCJP.txt',
                    'JAK': 'JEFASCK.txt',
                    'KEIS': 'KEISASCK.txt',
                    'KR': 'EBCASCUS.txt'
                },
                'ASCII_TO_EBCDIC': {
                    'US': 'ASCEBCUS.txt',
                    'JP': 'ASCEBCJP.txt',
                    'JAK': 'ASCJEFK.txt',
                    'KEIS': 'ASCJEISK.txt',
                    'KR': 'ASCEBCUS.txt'
                }
            }
        }
    
    @property
    def logging_config(self) -> Dict[str, Any]:
        """Logging configuration"""
        return {
            'level': os.environ.get('LOG_LEVEL', 'INFO'),
            'file': os.environ.get('LOG_FILE', 'conversion_service.log'),
            'format': os.environ.get('LOG_FORMAT', '%(asctime)s - %(name)s - %(levelname)s - %(message)s')
        }
    
    @property
    def conversion_config(self) -> Dict[str, Any]:
        """Conversion service configuration"""
        return {
            'default_encoding': os.environ.get('DEFAULT_ENCODING', 'US'),
            'default_record_length': int(os.environ.get('DEFAULT_RECORD_LENGTH', '80')),
            'max_input_size': int(os.environ.get('MAX_INPUT_SIZE', '1048576')),
            'timeout': int(os.environ.get('CONVERSION_TIMEOUT', '30'))
        }
    
    @property
    def security_config(self) -> Dict[str, Any]:
        """Security configuration"""
        cors_origins = os.environ.get('CORS_ORIGINS', '').split(',')
        return {
            'cors_origins': [origin.strip() for origin in cors_origins if origin.strip()],
            'api_key_required': os.environ.get('API_KEY_REQUIRED', 'false').lower() == 'true',
            'api_key': os.environ.get('API_KEY', '')
        }
    
    @property
    def performance_config(self) -> Dict[str, Any]:
        """Performance configuration"""
        return {
            'worker_processes': int(os.environ.get('WORKER_PROCESSES', '4')),
            'thread_pool_size': int(os.environ.get('THREAD_POOL_SIZE', '10')),
            'request_timeout': int(os.environ.get('REQUEST_TIMEOUT', '300'))
        }
    
    def get_codepage_file_path(self, encoding: str, direction: str) -> Path:
        """Get the full path to a codepage file"""
        codepage_files = self.codepage_config['files'][direction]
        filename = codepage_files.get(encoding, codepage_files['US'])
        return self.codepage_config['base_path'] / filename


# Global configuration instance
config = Config()