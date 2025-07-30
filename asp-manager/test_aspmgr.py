#!/usr/bin/env python3
"""Test script for ASP Manager."""

import sys
import os
import tempfile
import shutil
import time
from unittest.mock import patch, MagicMock

# Add the aspmgr module to path
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

def test_config():
    """Test configuration loading."""
    print("Testing configuration loading...")
    
    # Set test environment variables
    os.environ['ASPMGR_MIN_WIDTH'] = '100'
    os.environ['ASPMGR_MIN_HEIGHT'] = '30'
    os.environ['ASPMGR_COLOR_SCHEME'] = 'dark'
    os.environ['ASPMGR_ENABLE_LOGGING'] = 'false'  # Disable logging for tests
    
    from aspmgr.config import config
    
    # Test configuration values
    assert config.ui.min_width == 100
    assert config.ui.min_height == 30
    assert config.ui.color_scheme == 'dark'
    
    # Test validation
    errors = config.validate()
    assert len(errors) == 0, f"Configuration validation failed: {errors}"
    
    print("[OK] Configuration test passed")


def test_system_monitor():
    """Test system monitor."""
    print("Testing system monitor...")
    
    from aspmgr.utils import SystemMonitor
    
    monitor = SystemMonitor()
    
    # Test system status
    status = monitor.get_system_status()
    assert 'hostname' in status
    assert 'uptime' in status
    assert 'cpu_usage' in status
    assert isinstance(status['cpu_usage'], float)
    
    # Test performance metrics
    metrics = monitor.get_performance_metrics()
    assert 'cpu' in metrics
    assert 'memory' in metrics
    assert isinstance(metrics['cpu'], float)
    
    # Test system info
    info = monitor.get_system_info()
    assert 'os' in info
    assert 'cpu' in info
    assert 'memory' in info
    
    print("[OK] System monitor test passed")


def test_process_manager():
    """Test process manager."""
    print("Testing process manager...")
    
    from aspmgr.utils import ProcessManager
    
    manager = ProcessManager()
    
    # Test process list
    processes = manager.get_process_list()
    assert isinstance(processes, list)
    
    if processes:
        proc = processes[0]
        assert 'pid' in proc
        assert 'name' in proc
        assert 'status' in proc
        
        # Test process info
        info = manager.get_process_info(proc['pid'])
        if info:
            assert 'pid' in info
            assert 'name' in info
            assert 'status' in info
    
    print("[OK] Process manager test passed")


def test_logger():
    """Test logger."""
    print("Testing logger...")
    
    from aspmgr.utils import Logger
    
    logger = Logger("test")
    
    # Test logging (only in memory since file logging is disabled)
    logger.info("Test info message")
    logger.warning("Test warning message")
    logger.error("Test error message")
    
    # Test log retrieval
    logs = logger.get_logs()
    assert len(logs) >= 3
    
    # Test filtering
    error_logs = logger.get_logs(level="ERROR")
    assert len(error_logs) >= 1
    
    # Test search
    search_logs = logger.get_logs(search="warning")
    assert len(search_logs) >= 1
    
    print("[OK] Logger test passed")


def test_ui_components():
    """Test UI components (without curses)."""
    print("Testing UI components...")
    
    # Mock curses
    with patch('curses.newwin') as mock_newwin, \
         patch('curses.color_pair') as mock_color_pair, \
         patch('curses.A_BOLD', 1), \
         patch('curses.A_REVERSE', 2), \
         patch('curses.A_DIM', 4):
        
        # Create mock window
        mock_window = MagicMock()
        mock_newwin.return_value = mock_window
        mock_color_pair.return_value = 1
        
        from aspmgr.ui.base import Window, ScrollableWindow, MenuWindow
        
        # Test basic window
        window = Window(10, 20, 0, 0, "Test Window")
        assert window.height == 10
        assert window.width == 20
        assert window.title == "Test Window"
        
        # Test scrollable window
        scroll_window = ScrollableWindow(10, 20, 0, 0, "Scroll Test")
        scroll_window.set_content(["Line 1", "Line 2", "Line 3"])
        assert len(scroll_window.content) == 3
        
        # Test menu window
        menu_items = [("Item 1", lambda: None), ("Item 2", lambda: None)]
        menu = MenuWindow(10, 20, 0, 0, "Menu Test", menu_items)
        assert len(menu.items) == 2
        
    print("[OK] UI components test passed")


def test_constants():
    """Test constants."""
    print("Testing constants...")
    
    from aspmgr.constants import Colors, Keys, MenuItems, ProcessStatus
    
    # Test enum values
    assert Colors.DEFAULT == 1
    assert Colors.HEADER == 2
    
    assert Keys.ENTER == 10
    assert Keys.ESC == 27
    
    assert MenuItems.SYSTEM_STATUS.value == "System Status"
    assert MenuItems.EXIT.value == "Exit"
    
    assert ProcessStatus.RUNNING.value == "Running"
    assert ProcessStatus.STOPPED.value == "Stopped"
    
    print("[OK] Constants test passed")


def main():
    """Run all tests."""
    print("Running ASP Manager tests...\n")
    
    try:
        test_config()
        test_system_monitor()
        test_process_manager()
        test_logger()
        test_ui_components()
        test_constants()
        
        print("\n[OK] All tests passed!")
        return 0
        
    except Exception as e:
        print(f"\n[ERROR] Test failed: {e}")
        import traceback
        traceback.print_exc()
        return 1


if __name__ == '__main__':
    sys.exit(main())