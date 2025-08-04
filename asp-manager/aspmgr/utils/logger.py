"""Logging utilities for ASP Manager."""

import os
import logging
import threading
from datetime import datetime
from typing import List, Optional
from ..config import config
from ..constants import LogLevel


class Logger:
    """Application logger with file and memory storage."""
    
    def __init__(self, name: str = "aspmgr"):
        self.name = name
        self.memory_logs = []
        self.max_memory_logs = 1000
        self.lock = threading.Lock()
        
        # Create log directory if needed
        if config.system.enable_logging:
            os.makedirs(config.system.log_directory, exist_ok=True)
            
        # Setup file logger
        self._setup_file_logger()
        
    def _setup_file_logger(self):
        """Setup file-based logging."""
        if not config.system.enable_logging:
            return
            
        # Create logger
        self.file_logger = logging.getLogger(self.name)
        self.file_logger.setLevel(logging.DEBUG)
        
        # Create file handler
        log_file = os.path.join(config.system.log_directory, 
                               f"{self.name}_{datetime.now().strftime('%Y%m%d')}.log")
        
        handler = logging.FileHandler(log_file)
        handler.setLevel(logging.DEBUG)
        
        # Create formatter
        formatter = logging.Formatter(
            '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
        )
        handler.setFormatter(formatter)
        
        # Add handler
        self.file_logger.addHandler(handler)
        
    def _log(self, level: str, message: str, **kwargs):
        """Internal log method."""
        timestamp = datetime.now()
        
        # Add to memory logs
        with self.lock:
            self.memory_logs.append({
                'timestamp': timestamp,
                'level': level,
                'message': message,
                'extra': kwargs
            })
            
            # Limit memory logs
            if len(self.memory_logs) > self.max_memory_logs:
                self.memory_logs = self.memory_logs[-self.max_memory_logs:]
                
        # Log to file
        if config.system.enable_logging and hasattr(self, 'file_logger'):
            log_method = getattr(self.file_logger, level.lower())
            log_method(message, extra=kwargs)
            
    def debug(self, message: str, **kwargs):
        """Log debug message."""
        self._log(LogLevel.DEBUG.value, message, **kwargs)
        
    def info(self, message: str, **kwargs):
        """Log info message."""
        self._log(LogLevel.INFO.value, message, **kwargs)
        
    def warning(self, message: str, **kwargs):
        """Log warning message."""
        self._log(LogLevel.WARNING.value, message, **kwargs)
        
    def error(self, message: str, **kwargs):
        """Log error message."""
        self._log(LogLevel.ERROR.value, message, **kwargs)
        
    def critical(self, message: str, **kwargs):
        """Log critical message."""
        self._log(LogLevel.CRITICAL.value, message, **kwargs)
        
    def get_logs(self, level: Optional[str] = None, 
                 search: Optional[str] = None,
                 limit: int = 100) -> List[dict]:
        """Get logs from memory."""
        with self.lock:
            logs = self.memory_logs.copy()
            
        # Apply filters
        if level:
            logs = [log for log in logs if log['level'] == level]
            
        if search:
            search_lower = search.lower()
            logs = [log for log in logs 
                   if search_lower in log['message'].lower()]
                   
        # Apply limit
        if limit and len(logs) > limit:
            logs = logs[-limit:]
            
        return logs
        
    def clear_logs(self):
        """Clear memory logs."""
        with self.lock:
            self.memory_logs = []
            
    def rotate_logs(self):
        """Rotate log files."""
        if not config.system.enable_logging:
            return
            
        # Close current handler
        if hasattr(self, 'file_logger'):
            for handler in self.file_logger.handlers:
                handler.close()
                self.file_logger.removeHandler(handler)
                
        # Setup new handler
        self._setup_file_logger()
        
    def get_log_files(self) -> List[str]:
        """Get list of log files."""
        if not config.system.enable_logging:
            return []
            
        log_files = []
        
        try:
            for filename in os.listdir(config.system.log_directory):
                if filename.startswith(self.name) and filename.endswith('.log'):
                    filepath = os.path.join(config.system.log_directory, filename)
                    log_files.append(filepath)
                    
            # Sort by modification time
            log_files.sort(key=lambda x: os.path.getmtime(x), reverse=True)
            
        except Exception:
            pass
            
        return log_files