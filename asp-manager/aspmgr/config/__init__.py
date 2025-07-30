"""Configuration module for ASP Manager CUI application."""

import os
from dataclasses import dataclass, field
from typing import Dict, List, Optional


@dataclass
class UIConfig:
    """User interface configuration settings."""
    
    # Terminal settings
    min_width: int = int(os.getenv('ASPMGR_MIN_WIDTH', '80'))
    min_height: int = int(os.getenv('ASPMGR_MIN_HEIGHT', '24'))
    
    # Color scheme (uses curses color pair numbers)
    color_scheme: str = os.getenv('ASPMGR_COLOR_SCHEME', 'default')
    enable_mouse: bool = os.getenv('ASPMGR_ENABLE_MOUSE', 'true').lower() == 'true'
    
    # Window layout percentages
    header_height: int = int(os.getenv('ASPMGR_HEADER_HEIGHT', '3'))
    footer_height: int = int(os.getenv('ASPMGR_FOOTER_HEIGHT', '3'))
    sidebar_width_percent: int = int(os.getenv('ASPMGR_SIDEBAR_WIDTH_PERCENT', '25'))
    
    # Refresh intervals (milliseconds)
    status_refresh_interval: int = int(os.getenv('ASPMGR_STATUS_REFRESH_MS', '5000'))
    metrics_refresh_interval: int = int(os.getenv('ASPMGR_METRICS_REFRESH_MS', '3000'))
    
    # Scrolling
    scroll_page_size: int = int(os.getenv('ASPMGR_SCROLL_PAGE_SIZE', '10'))
    scroll_buffer_size: int = int(os.getenv('ASPMGR_SCROLL_BUFFER_SIZE', '1000'))


@dataclass
class SystemConfig:
    """System configuration settings."""
    
    # Paths
    log_directory: str = os.getenv('ASPMGR_LOG_DIR', '/var/log/aspmgr')
    config_directory: str = os.getenv('ASPMGR_CONFIG_DIR', '/etc/aspmgr')
    temp_directory: str = os.getenv('ASPMGR_TEMP_DIR', '/tmp/aspmgr')
    
    # Process management
    max_processes: int = int(os.getenv('ASPMGR_MAX_PROCESSES', '100'))
    process_check_interval: int = int(os.getenv('ASPMGR_PROCESS_CHECK_INTERVAL', '2'))
    
    # Performance
    cache_size: int = int(os.getenv('ASPMGR_CACHE_SIZE', '100'))
    history_size: int = int(os.getenv('ASPMGR_HISTORY_SIZE', '50'))
    
    # Features
    enable_logging: bool = os.getenv('ASPMGR_ENABLE_LOGGING', 'true').lower() == 'true'
    enable_metrics: bool = os.getenv('ASPMGR_ENABLE_METRICS', 'true').lower() == 'true'
    enable_notifications: bool = os.getenv('ASPMGR_ENABLE_NOTIFICATIONS', 'true').lower() == 'true'


@dataclass
class SecurityConfig:
    """Security configuration settings."""
    
    # Authentication
    auth_timeout: int = int(os.getenv('ASPMGR_AUTH_TIMEOUT', '300'))
    max_login_attempts: int = int(os.getenv('ASPMGR_MAX_LOGIN_ATTEMPTS', '3'))
    
    # Permissions
    enable_permission_check: bool = os.getenv('ASPMGR_ENABLE_PERMISSION_CHECK', 'true').lower() == 'true'
    admin_only_features: List[str] = field(default_factory=lambda: os.getenv('ASPMGR_ADMIN_ONLY_FEATURES', 'shutdown,config_edit').split(','))


@dataclass
class AppConfig:
    """Main application configuration."""
    
    ui: UIConfig
    system: SystemConfig
    security: SecurityConfig
    
    # Application info
    app_name: str = os.getenv('ASPMGR_APP_NAME', 'ASP Manager')
    version: str = os.getenv('ASPMGR_VERSION', '1.0.0')
    environment: str = os.getenv('ASPMGR_ENV', 'production')
    
    @classmethod
    def load(cls) -> 'AppConfig':
        """Load configuration from environment variables."""
        return cls(
            ui=UIConfig(),
            system=SystemConfig(),
            security=SecurityConfig()
        )
    
    def validate(self) -> List[str]:
        """Validate configuration and return list of errors."""
        errors = []
        
        # Validate UI settings
        if self.ui.min_width < 80:
            errors.append("Minimum width must be at least 80 characters")
        if self.ui.min_height < 24:
            errors.append("Minimum height must be at least 24 lines")
        if not 10 <= self.ui.sidebar_width_percent <= 50:
            errors.append("Sidebar width must be between 10% and 50%")
        
        # Validate system settings
        if self.system.max_processes < 1:
            errors.append("Max processes must be at least 1")
        if self.system.cache_size < 0:
            errors.append("Cache size cannot be negative")
        
        # Validate security settings
        if self.security.auth_timeout < 60:
            errors.append("Auth timeout must be at least 60 seconds")
        if self.security.max_login_attempts < 1:
            errors.append("Max login attempts must be at least 1")
        
        return errors


# Global configuration instance
config = AppConfig.load()