#!/usr/bin/env python3
"""
ASP Manager Test Suite
"""

import unittest
import sys
import os

# Add the current directory to Python path
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from aspmgr.config import ConfigManager, get_config
from aspmgr.system_monitor import SystemMonitor, get_system_monitor
from aspmgr.ui_base import Rect, Position, Size


class TestConfig(unittest.TestCase):
    """Test configuration management"""
    
    def setUp(self):
        # Set test environment variables
        os.environ['ASPMGR_DEMO_MODE'] = 'true'
        os.environ['ASPMGR_DEBUG'] = 'false'
        os.environ['ASPMGR_MIN_WIDTH'] = '80'
        os.environ['ASPMGR_MIN_HEIGHT'] = '24'
    
    def test_config_loading(self):
        """Test configuration loading"""
        config_manager = ConfigManager()
        config = config_manager.get_config()
        
        self.assertEqual(config.terminal.min_width, 80)
        self.assertEqual(config.terminal.min_height, 24)
        self.assertTrue(config.demo_mode)
        self.assertFalse(config.debug_mode)
    
    def test_config_validation(self):
        """Test configuration validation"""
        # Save original values
        original_width = os.environ.get('ASPMGR_MIN_WIDTH', '80')
        original_threshold = os.environ.get('ASPMGR_MEMORY_THRESHOLD', '80')
        
        # Test invalid terminal size
        os.environ['ASPMGR_MIN_WIDTH'] = '50'
        with self.assertRaises(ValueError):
            ConfigManager()
        
        # Reset to valid value
        os.environ['ASPMGR_MIN_WIDTH'] = original_width
        
        # Test invalid threshold
        os.environ['ASPMGR_MEMORY_THRESHOLD'] = '150'
        with self.assertRaises(ValueError):
            ConfigManager()
        
        # Reset to valid value
        os.environ['ASPMGR_MEMORY_THRESHOLD'] = original_threshold
    
    def test_color_pairs(self):
        """Test color pair generation"""
        config_manager = ConfigManager()
        color_pairs = config_manager.get_color_pairs()
        
        self.assertIn('normal', color_pairs)
        self.assertIn('header', color_pairs)
        self.assertIn('selected', color_pairs)
        self.assertIsInstance(color_pairs['normal'], tuple)
        self.assertEqual(len(color_pairs['normal']), 2)
    
    def test_unicode_chars(self):
        """Test Unicode character mapping"""
        config_manager = ConfigManager()
        unicode_chars = config_manager.get_unicode_chars()
        
        self.assertIn('h_line', unicode_chars)
        self.assertIn('v_line', unicode_chars)
        self.assertIn('tl_corner', unicode_chars)
        self.assertIsInstance(unicode_chars['h_line'], str)


class TestSystemMonitor(unittest.TestCase):
    """Test system monitoring"""
    
    def setUp(self):
        os.environ['ASPMGR_DEMO_MODE'] = 'true'
        self.monitor = SystemMonitor()
    
    def test_system_info(self):
        """Test system information retrieval"""
        info = self.monitor.get_system_info()
        
        self.assertIsNotNone(info)
        self.assertIsInstance(info.hostname, str)
        self.assertIsInstance(info.uptime, float)
        self.assertIsInstance(info.cpu_percent, (int, float))
        self.assertIsInstance(info.memory_percent, (int, float))
        self.assertIsInstance(info.disk_percent, (int, float))
        
        # Check valid ranges
        self.assertGreaterEqual(float(info.cpu_percent), 0)
        self.assertLessEqual(float(info.cpu_percent), 100)
        self.assertGreaterEqual(float(info.memory_percent), 0)
        self.assertLessEqual(float(info.memory_percent), 100)
        self.assertGreaterEqual(float(info.disk_percent), 0)
        self.assertLessEqual(float(info.disk_percent), 100)
    
    def test_process_list(self):
        """Test process list retrieval"""
        processes = self.monitor.get_process_list()
        
        self.assertIsInstance(processes, list)
        if processes:
            proc = processes[0]
            self.assertIsInstance(proc.pid, int)
            self.assertIsInstance(proc.name, str)
            self.assertIsInstance(proc.cpu_percent, float)
            self.assertIsInstance(proc.memory_percent, float)
    
    def test_format_functions(self):
        """Test formatting functions"""
        # Test byte formatting
        self.assertEqual(self.monitor.format_bytes(1024), "1.0 KB")
        self.assertEqual(self.monitor.format_bytes(1024 * 1024), "1.0 MB")
        self.assertEqual(self.monitor.format_bytes(1024 * 1024 * 1024), "1.0 GB")
        
        # Test uptime formatting
        self.assertEqual(self.monitor.format_uptime(3600), "1h 0m")
        self.assertEqual(self.monitor.format_uptime(86400), "1d 0h 0m")
        self.assertEqual(self.monitor.format_uptime(300), "5m")
    
    def test_alert_conditions(self):
        """Test alert condition checking"""
        info = self.monitor.get_system_info()
        alerts = self.monitor.is_alert_condition(info)
        
        self.assertIsInstance(alerts, list)


class TestUIBase(unittest.TestCase):
    """Test UI base components"""
    
    def test_position(self):
        """Test Position class"""
        pos = Position(10, 20)
        self.assertEqual(pos.y, 10)
        self.assertEqual(pos.x, 20)
    
    def test_size(self):
        """Test Size class"""
        size = Size(30, 40)
        self.assertEqual(size.height, 30)
        self.assertEqual(size.width, 40)
    
    def test_rect(self):
        """Test Rect class"""
        rect = Rect(Position(5, 10), Size(20, 30))
        self.assertEqual(rect.y, 5)
        self.assertEqual(rect.x, 10)
        self.assertEqual(rect.height, 20)
        self.assertEqual(rect.width, 30)
        self.assertEqual(rect.right, 40)
        self.assertEqual(rect.bottom, 25)


class TestGlobalFunctions(unittest.TestCase):
    """Test global functions"""
    
    def test_get_config(self):
        """Test get_config function"""
        config = get_config()
        self.assertIsNotNone(config)
        self.assertIsNotNone(config.terminal)
        self.assertIsNotNone(config.system)
        self.assertIsNotNone(config.ui)
    
    def test_get_system_monitor(self):
        """Test get_system_monitor function"""
        monitor = get_system_monitor()
        self.assertIsNotNone(monitor)
        self.assertIsInstance(monitor, SystemMonitor)
        
        # Test singleton behavior
        monitor2 = get_system_monitor()
        self.assertIs(monitor, monitor2)


def run_tests():
    """Run all tests"""
    # Set up test environment
    os.environ['ASPMGR_DEMO_MODE'] = 'true'
    os.environ['ASPMGR_DEBUG'] = 'false'
    os.environ['ASPMGR_MIN_WIDTH'] = '80'
    os.environ['ASPMGR_MIN_HEIGHT'] = '24'
    os.environ['ASPMGR_COLOR_SCHEME'] = 'default'
    os.environ['ASPMGR_MEMORY_THRESHOLD'] = '80'
    os.environ['ASPMGR_CPU_THRESHOLD'] = '80'
    os.environ['ASPMGR_DISK_THRESHOLD'] = '90'
    
    # Create test suite
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    
    # Add test cases
    suite.addTests(loader.loadTestsFromTestCase(TestConfig))
    suite.addTests(loader.loadTestsFromTestCase(TestSystemMonitor))
    suite.addTests(loader.loadTestsFromTestCase(TestUIBase))
    suite.addTests(loader.loadTestsFromTestCase(TestGlobalFunctions))
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Return success status
    return result.wasSuccessful()


if __name__ == "__main__":
    success = run_tests()
    sys.exit(0 if success else 1)