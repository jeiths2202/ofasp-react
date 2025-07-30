#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Logging utility following CODING_RULES.md
Configurable logging without hardcoded values
"""

import logging
import logging.handlers
from pathlib import Path
from typing import Optional

from config.config import config


class Logger:
    """Configurable logger utility"""
    
    def __init__(self, name: str, log_file: Optional[str] = None):
        """
        Initialize logger with configuration
        
        Args:
            name: Logger name
            log_file: Optional custom log file path
        """
        self.name = name
        self.logger = logging.getLogger(name)
        self._setup_logger(log_file)
    
    def _setup_logger(self, log_file: Optional[str] = None):
        """Setup logger with configuration"""
        # Clear existing handlers
        self.logger.handlers.clear()
        
        # Set log level
        log_config = config.logging_config
        level = getattr(logging, log_config['level'].upper(), logging.INFO)
        self.logger.setLevel(level)
        
        # Create formatter
        formatter = logging.Formatter(log_config['format'])
        
        # Console handler
        console_handler = logging.StreamHandler()
        console_handler.setFormatter(formatter)
        self.logger.addHandler(console_handler)
        
        # File handler
        if log_file is None:
            log_file = log_config['file']
        
        try:
            # Create log directory if it doesn't exist
            log_path = Path(log_file)
            log_path.parent.mkdir(parents=True, exist_ok=True)
            
            # Rotating file handler
            file_handler = logging.handlers.RotatingFileHandler(
                log_file,
                maxBytes=10*1024*1024,  # 10MB
                backupCount=5
            )
            file_handler.setFormatter(formatter)
            self.logger.addHandler(file_handler)
        except Exception as e:
            self.logger.warning(f"Could not setup file logging: {e}")
    
    def debug(self, message: str, *args, **kwargs):
        """Log debug message"""
        self.logger.debug(message, *args, **kwargs)
    
    def info(self, message: str, *args, **kwargs):
        """Log info message"""
        self.logger.info(message, *args, **kwargs)
    
    def warning(self, message: str, *args, **kwargs):
        """Log warning message"""
        self.logger.warning(message, *args, **kwargs)
    
    def error(self, message: str, *args, **kwargs):
        """Log error message"""
        self.logger.error(message, *args, **kwargs)
    
    def critical(self, message: str, *args, **kwargs):
        """Log critical message"""
        self.logger.critical(message, *args, **kwargs)
    
    def exception(self, message: str, *args, **kwargs):
        """Log exception with traceback"""
        self.logger.exception(message, *args, **kwargs)


# Global logger instances
conversion_logger = Logger('conversion_service')
api_logger = Logger('api_service')

# Export for easy import
__all__ = ['Logger', 'conversion_logger', 'api_logger']