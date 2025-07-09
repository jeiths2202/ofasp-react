#!/usr/bin/env python3
"""
ASP Manager Configuration Module
Manages all configuration settings through environment variables
"""

import os
from dataclasses import dataclass
from typing import Dict, List, Optional, Union
from enum import Enum


class ColorScheme(Enum):
    """Available color schemes for the ASP Manager interface"""
    DEFAULT = "default"
    DARK = "dark"
    LIGHT = "light"
    BLUE = "blue"
    GREEN = "green"


@dataclass
class TerminalConfig:
    """Terminal display configuration"""
    min_width: int
    min_height: int
    color_scheme: ColorScheme
    use_unicode: bool
    mouse_support: bool
    refresh_interval: float


@dataclass
class SystemConfig:
    """System monitoring configuration"""
    log_directory: str
    max_log_files: int
    log_retention_days: int
    process_scan_interval: float
    memory_threshold: float
    cpu_threshold: float
    disk_threshold: float


@dataclass
class UIConfig:
    """User interface configuration"""
    title: str
    version: str
    copyright: str
    help_text: str
    status_format: str
    time_format: str


@dataclass
class NetworkConfig:
    """Network monitoring configuration"""
    interfaces: List[str]
    timeout: float
    max_connections: int


@dataclass
class ASPManagerConfig:
    """Main configuration container"""
    terminal: TerminalConfig
    system: SystemConfig
    ui: UIConfig
    network: NetworkConfig
    debug_mode: bool
    demo_mode: bool


class ConfigManager:
    """Manages configuration loading and validation"""
    
    def __init__(self):
        self.config = self._load_config()
        self._validate_config()
    
    def _load_config(self) -> ASPManagerConfig:
        """Load configuration from environment variables"""
        
        # Terminal configuration
        terminal_config = TerminalConfig(
            min_width=int(os.getenv('ASPMGR_MIN_WIDTH', '80')),
            min_height=int(os.getenv('ASPMGR_MIN_HEIGHT', '24')),
            color_scheme=ColorScheme(os.getenv('ASPMGR_COLOR_SCHEME', 'default')),
            use_unicode=os.getenv('ASPMGR_USE_UNICODE', 'true').lower() == 'true',
            mouse_support=os.getenv('ASPMGR_MOUSE_SUPPORT', 'true').lower() == 'true',
            refresh_interval=float(os.getenv('ASPMGR_REFRESH_INTERVAL', '1.0'))
        )
        
        # System configuration
        system_config = SystemConfig(
            log_directory=os.getenv('ASPMGR_LOG_DIR', '/var/log/aspmgr'),
            max_log_files=int(os.getenv('ASPMGR_MAX_LOG_FILES', '10')),
            log_retention_days=int(os.getenv('ASPMGR_LOG_RETENTION_DAYS', '30')),
            process_scan_interval=float(os.getenv('ASPMGR_PROCESS_SCAN_INTERVAL', '2.0')),
            memory_threshold=float(os.getenv('ASPMGR_MEMORY_THRESHOLD', '80.0')),
            cpu_threshold=float(os.getenv('ASPMGR_CPU_THRESHOLD', '80.0')),
            disk_threshold=float(os.getenv('ASPMGR_DISK_THRESHOLD', '90.0'))
        )
        
        # UI configuration
        ui_config = UIConfig(
            title=os.getenv('ASPMGR_TITLE', 'ASP Manager'),
            version=os.getenv('ASPMGR_VERSION', '1.0.0'),
            copyright=os.getenv('ASPMGR_COPYRIGHT', 'Copyright (C) 2025 ASP Manager'),
            help_text=os.getenv('ASPMGR_HELP_TEXT', 'F1:Help F2:Refresh F3:Exit'),
            status_format=os.getenv('ASPMGR_STATUS_FORMAT', '%Y-%m-%d %H:%M:%S'),
            time_format=os.getenv('ASPMGR_TIME_FORMAT', '%H:%M:%S')
        )
        
        # Network configuration
        network_config = NetworkConfig(
            interfaces=os.getenv('ASPMGR_NETWORK_INTERFACES', 'eth0,lo').split(','),
            timeout=float(os.getenv('ASPMGR_NETWORK_TIMEOUT', '5.0')),
            max_connections=int(os.getenv('ASPMGR_MAX_CONNECTIONS', '100'))
        )
        
        return ASPManagerConfig(
            terminal=terminal_config,
            system=system_config,
            ui=ui_config,
            network=network_config,
            debug_mode=os.getenv('ASPMGR_DEBUG', 'false').lower() == 'true',
            demo_mode=os.getenv('ASPMGR_DEMO_MODE', 'false').lower() == 'true'
        )
    
    def _validate_config(self) -> None:
        """Validate configuration values"""
        config = self.config
        
        # Validate terminal settings
        if config.terminal.min_width < 60:
            raise ValueError(f"Terminal width must be at least 60, got {config.terminal.min_width}")
        if config.terminal.min_height < 20:
            raise ValueError(f"Terminal height must be at least 20, got {config.terminal.min_height}")
        
        # Validate system settings
        if config.system.memory_threshold < 0 or config.system.memory_threshold > 100:
            raise ValueError(f"Memory threshold must be 0-100, got {config.system.memory_threshold}")
        if config.system.cpu_threshold < 0 or config.system.cpu_threshold > 100:
            raise ValueError(f"CPU threshold must be 0-100, got {config.system.cpu_threshold}")
        if config.system.disk_threshold < 0 or config.system.disk_threshold > 100:
            raise ValueError(f"Disk threshold must be 0-100, got {config.system.disk_threshold}")
        
        # Validate intervals
        if config.terminal.refresh_interval < 0.1:
            raise ValueError(f"Refresh interval must be at least 0.1s, got {config.terminal.refresh_interval}")
        if config.system.process_scan_interval < 0.5:
            raise ValueError(f"Process scan interval must be at least 0.5s, got {config.system.process_scan_interval}")
    
    def get_config(self) -> ASPManagerConfig:
        """Get the current configuration"""
        return self.config
    
    def get_color_pairs(self) -> Dict[str, tuple]:
        """Get color pairs for the selected scheme"""
        scheme = self.config.terminal.color_scheme
        
        color_schemes = {
            ColorScheme.DEFAULT: {
                'normal': (7, 0),        # white on black
                'header': (15, 4),       # bright white on red
                'menu': (0, 7),          # black on white
                'selected': (0, 3),      # black on yellow
                'status': (10, 0),       # bright green on black
                'error': (9, 0),         # bright red on black
                'warning': (11, 0),      # bright yellow on black
                'border': (8, 0),        # gray on black
                'info': (14, 0),         # bright cyan on black
                'success': (10, 0),      # bright green on black
            },
            ColorScheme.DARK: {
                'normal': (7, 0),
                'header': (15, 8),
                'menu': (7, 8),
                'selected': (15, 4),
                'status': (10, 8),
                'error': (9, 8),
                'warning': (11, 8),
                'border': (8, 0),
                'info': (14, 8),
                'success': (10, 8),
            },
            ColorScheme.LIGHT: {
                'normal': (0, 7),
                'header': (0, 15),
                'menu': (0, 15),
                'selected': (15, 4),
                'status': (0, 7),
                'error': (9, 7),
                'warning': (11, 7),
                'border': (8, 7),
                'info': (4, 7),
                'success': (2, 7),
            },
            ColorScheme.BLUE: {
                'normal': (15, 4),
                'header': (15, 1),
                'menu': (15, 4),
                'selected': (15, 6),
                'status': (14, 4),
                'error': (9, 4),
                'warning': (11, 4),
                'border': (8, 4),
                'info': (14, 4),
                'success': (10, 4),
            },
            ColorScheme.GREEN: {
                'normal': (15, 2),
                'header': (15, 10),
                'menu': (15, 2),
                'selected': (0, 10),
                'status': (15, 2),
                'error': (9, 2),
                'warning': (11, 2),
                'border': (8, 2),
                'info': (14, 2),
                'success': (10, 2),
            }
        }
        
        return color_schemes.get(scheme, color_schemes[ColorScheme.DEFAULT])
    
    def get_unicode_chars(self) -> Dict[str, str]:
        """Get Unicode characters for drawing"""
        if self.config.terminal.use_unicode:
            return {
                'h_line': '─',
                'v_line': '│',
                'tl_corner': '┌',
                'tr_corner': '┐',
                'bl_corner': '└',
                'br_corner': '┘',
                'cross': '┼',
                't_down': '┬',
                't_up': '┴',
                't_right': '├',
                't_left': '┤',
                'bullet': '●',
                'arrow_right': '→',
                'arrow_left': '←',
                'arrow_up': '↑',
                'arrow_down': '↓',
                'check': '✓',
                'cross_mark': '✗',
                'warning': '⚠',
                'info': 'ℹ',
                'gear': '⚙',
                'clock': '⏰',
                'cpu': '⚡',
                'memory': '💾',
                'disk': '💿',
                'network': '🌐',
            }
        else:
            return {
                'h_line': '-',
                'v_line': '|',
                'tl_corner': '+',
                'tr_corner': '+',
                'bl_corner': '+',
                'br_corner': '+',
                'cross': '+',
                't_down': '+',
                't_up': '+',
                't_right': '+',
                't_left': '+',
                'bullet': '*',
                'arrow_right': '>',
                'arrow_left': '<',
                'arrow_up': '^',
                'arrow_down': 'v',
                'check': 'Y',
                'cross_mark': 'X',
                'warning': '!',
                'info': 'i',
                'gear': 'S',
                'clock': 'T',
                'cpu': 'C',
                'memory': 'M',
                'disk': 'D',
                'network': 'N',
            }


# Global configuration instance
_config_manager = None


def get_config_manager() -> ConfigManager:
    """Get the global configuration manager instance"""
    global _config_manager
    if _config_manager is None:
        _config_manager = ConfigManager()
    return _config_manager


def get_config() -> ASPManagerConfig:
    """Get the current configuration"""
    return get_config_manager().get_config()


def get_color_pairs() -> Dict[str, tuple]:
    """Get color pairs for the current scheme"""
    return get_config_manager().get_color_pairs()


def get_unicode_chars() -> Dict[str, str]:
    """Get Unicode characters for drawing"""
    return get_config_manager().get_unicode_chars()