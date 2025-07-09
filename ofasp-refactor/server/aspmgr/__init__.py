#!/usr/bin/env python3
"""
ASP Manager - Curses-based System Management Tool
"""

__version__ = "1.0.0"
__author__ = "ASP Manager Development Team"
__license__ = "MIT"

from .aspmgr import ASPManager, main
from .config import get_config, get_config_manager
from .system_monitor import get_system_monitor
from .ui_base import BaseWindow, ScrollableWindow, MenuWindow

__all__ = [
    'ASPManager',
    'main',
    'get_config',
    'get_config_manager',
    'get_system_monitor',
    'BaseWindow',
    'ScrollableWindow',
    'MenuWindow',
]