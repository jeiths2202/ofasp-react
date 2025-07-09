#!/usr/bin/env python3
"""
ASP Manager - Main Application
Curses-based system management tool
"""

import curses
import sys
import signal
import time
import threading
import logging
from typing import Optional, List, Dict, Any
from datetime import datetime
from enum import Enum

from .config import get_config, get_color_pairs, get_unicode_chars
from .ui_base import BaseWindow, ScrollableWindow, MenuWindow, Rect, Position, Size, WindowType
from .system_monitor import get_system_monitor, SystemInfo, ProcessInfo


class AppState(Enum):
    """Application states"""
    RUNNING = "running"
    EXITING = "exiting"
    ERROR = "error"


class MainScreen(Enum):
    """Main screen types"""
    DASHBOARD = "dashboard"
    PROCESSES = "processes"
    LOGS = "logs"
    CONFIG = "config"
    HELP = "help"


class ASPManager:
    """Main ASP Manager application"""
    
    def __init__(self):
        self.config = get_config()
        self.color_pairs = get_color_pairs()
        self.unicode_chars = get_unicode_chars()
        self.system_monitor = get_system_monitor()
        
        # Application state
        self.state = AppState.RUNNING
        self.current_screen = MainScreen.DASHBOARD
        self.last_update = datetime.now()
        
        # UI components
        self.stdscr: Optional[curses.window] = None
        self.title_window: Optional[BaseWindow] = None
        self.menu_window: Optional[MenuWindow] = None
        self.content_window: Optional[ScrollableWindow] = None
        self.status_window: Optional[BaseWindow] = None
        
        # Data
        self.system_info: Optional[SystemInfo] = None
        self.process_list: List[ProcessInfo] = []
        self.log_lines: List[str] = []
        
        # Threading
        self.update_thread: Optional[threading.Thread] = None
        self.update_lock = threading.Lock()
        
        # Setup logging
        self._setup_logging()
    
    def _setup_logging(self):
        """Setup logging configuration"""
        if self.config.debug_mode:
            logging.basicConfig(
                level=logging.DEBUG,
                format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
                handlers=[
                    logging.FileHandler('/tmp/aspmgr.log'),
                    logging.StreamHandler()
                ]
            )
        else:
            logging.basicConfig(level=logging.WARNING)
        
        self.logger = logging.getLogger('ASPManager')
    
    def run(self):
        """Main application entry point"""
        try:
            # Initialize curses
            self.stdscr = curses.initscr()
            self._setup_curses()
            
            # Setup signal handlers
            signal.signal(signal.SIGINT, self._signal_handler)
            signal.signal(signal.SIGTERM, self._signal_handler)
            
            # Check terminal size
            if not self._check_terminal_size():
                return
            
            # Initialize UI
            self._initialize_ui()
            
            # Start update thread
            self._start_update_thread()
            
            # Main loop
            self._main_loop()
            
        except Exception as e:
            self.logger.error(f"Application error: {e}")
            self.state = AppState.ERROR
        finally:
            self._cleanup()
    
    def _setup_curses(self):
        """Setup curses environment"""
        curses.noecho()
        curses.cbreak()
        curses.curs_set(0)  # Hide cursor
        
        if curses.has_colors():
            curses.start_color()
            curses.use_default_colors()
            
            # Initialize color pairs
            color_pairs = self.color_pairs
            for i, (name, (fg, bg)) in enumerate(color_pairs.items(), 1):
                try:
                    curses.init_pair(i, fg, bg)
                except curses.error:
                    pass
        
        # Enable keypad
        self.stdscr.keypad(True)
        self.stdscr.nodelay(True)
    
    def _check_terminal_size(self) -> bool:
        """Check if terminal size is adequate"""
        height, width = self.stdscr.getmaxyx()
        
        if width < self.config.terminal.min_width or height < self.config.terminal.min_height:
            self._show_size_error(width, height)
            return False
        
        return True
    
    def _show_size_error(self, width: int, height: int):
        """Show terminal size error message"""
        self.stdscr.clear()
        error_msg = [
            "Terminal size too small!",
            f"Current size: {width}x{height}",
            f"Required size: {self.config.terminal.min_width}x{self.config.terminal.min_height}",
            "",
            "Please resize your terminal and try again.",
            "Press any key to exit..."
        ]
        
        for i, line in enumerate(error_msg):
            try:
                self.stdscr.addstr(i + 1, 2, line)
            except curses.error:
                pass
        
        self.stdscr.refresh()
        self.stdscr.getch()
    
    def _initialize_ui(self):
        """Initialize UI components"""
        height, width = self.stdscr.getmaxyx()
        
        # Title bar (top 3 lines)
        self.title_window = TitleWindow(
            self.stdscr,
            Rect(Position(0, 0), Size(3, width)),
            self.config.ui.title,
            self.config.ui.version
        )
        
        # Menu bar (left side, 20 chars wide)
        menu_items = [
            f"{self.unicode_chars['gear']} Dashboard",
            f"{self.unicode_chars['cpu']} Processes", 
            f"{self.unicode_chars['info']} Logs",
            f"{self.unicode_chars['gear']} Config",
            f"{self.unicode_chars['info']} Help"
        ]
        
        self.menu_window = MenuWindow(
            self.stdscr,
            Rect(Position(3, 0), Size(height - 5, 20)),
            menu_items
        )
        
        # Content area (main display)
        self.content_window = ScrollableWindow(
            self.stdscr,
            Rect(Position(3, 20), Size(height - 5, width - 20))
        )
        
        # Status bar (bottom 2 lines)
        self.status_window = StatusWindow(
            self.stdscr,
            Rect(Position(height - 2, 0), Size(2, width))
        )
    
    def _start_update_thread(self):
        """Start the background update thread"""
        self.update_thread = threading.Thread(target=self._update_loop, daemon=True)
        self.update_thread.start()
    
    def _update_loop(self):
        """Background update loop"""
        while self.state == AppState.RUNNING:
            try:
                with self.update_lock:
                    self._update_data()
                time.sleep(self.config.terminal.refresh_interval)
            except Exception as e:
                self.logger.error(f"Update error: {e}")
    
    def _update_data(self):
        """Update system data"""
        self.system_info = self.system_monitor.get_system_info()
        
        if self.current_screen == MainScreen.PROCESSES:
            self.process_list = self.system_monitor.get_top_processes(20)
        elif self.current_screen == MainScreen.LOGS:
            self._update_log_data()
        
        self.last_update = datetime.now()
    
    def _update_log_data(self):
        """Update log data"""
        # Demo log data
        if self.config.demo_mode:
            self.log_lines = [
                f"[{datetime.now().strftime('%H:%M:%S')}] INFO: System monitoring active",
                f"[{datetime.now().strftime('%H:%M:%S')}] INFO: CPU usage: {self.system_info.cpu_percent:.1f}%",
                f"[{datetime.now().strftime('%H:%M:%S')}] INFO: Memory usage: {self.system_info.memory_percent:.1f}%",
                f"[{datetime.now().strftime('%H:%M:%S')}] INFO: Disk usage: {self.system_info.disk_percent:.1f}%",
            ]
    
    def _main_loop(self):
        """Main application loop"""
        while self.state == AppState.RUNNING:
            try:
                # Handle resize
                if curses.is_term_resized(self.stdscr.getmaxyx()[0], self.stdscr.getmaxyx()[1]):
                    self._handle_resize()
                
                # Update display
                self._update_display()
                
                # Handle input
                key = self.stdscr.getch()
                if key != -1:
                    self._handle_input(key)
                
                # Small delay to prevent high CPU usage
                time.sleep(0.05)
                
            except KeyboardInterrupt:
                self.state = AppState.EXITING
                break
            except Exception as e:
                self.logger.error(f"Main loop error: {e}")
    
    def _handle_resize(self):
        """Handle terminal resize"""
        curses.resizeterm(*self.stdscr.getmaxyx())
        self.stdscr.clear()
        
        if self._check_terminal_size():
            self._initialize_ui()
    
    def _update_display(self):
        """Update the display"""
        try:
            # Clear screen
            self.stdscr.clear()
            
            # Draw all windows
            if self.title_window:
                self.title_window.draw()
            
            if self.menu_window:
                self.menu_window.draw()
            
            if self.content_window:
                self._update_content_window()
                self.content_window.draw()
            
            if self.status_window:
                self.status_window.draw()
            
            # Refresh screen
            self.stdscr.refresh()
            
        except curses.error:
            pass
    
    def _update_content_window(self):
        """Update content window based on current screen"""
        if not self.content_window:
            return
        
        if self.current_screen == MainScreen.DASHBOARD:
            self._update_dashboard_content()
        elif self.current_screen == MainScreen.PROCESSES:
            self._update_process_content()
        elif self.current_screen == MainScreen.LOGS:
            self._update_log_content()
        elif self.current_screen == MainScreen.CONFIG:
            self._update_config_content()
        elif self.current_screen == MainScreen.HELP:
            self._update_help_content()
    
    def _update_dashboard_content(self):
        """Update dashboard content"""
        if not self.system_info:
            return
        
        lines = [
            f"System Dashboard - {self.system_info.timestamp.strftime('%Y-%m-%d %H:%M:%S')}",
            "",
            f"Hostname: {self.system_info.hostname}",
            f"Uptime: {self.system_monitor.format_uptime(self.system_info.uptime)}",
            f"Load Average: {self.system_info.load_avg[0]:.2f}, {self.system_info.load_avg[1]:.2f}, {self.system_info.load_avg[2]:.2f}",
            "",
            "System Resources:",
            f"  CPU Usage: {self.system_info.cpu_percent:.1f}%",
            f"  Memory: {self.system_info.memory_percent:.1f}% ({self.system_monitor.format_bytes(self.system_info.memory_used)} / {self.system_monitor.format_bytes(self.system_info.memory_total)})",
            f"  Disk: {self.system_info.disk_percent:.1f}% ({self.system_monitor.format_bytes(self.system_info.disk_used)} / {self.system_monitor.format_bytes(self.system_info.disk_total)})",
            "",
            f"Process Count: {self.system_info.process_count}",
            "",
            "Alerts:",
        ]
        
        alerts = self.system_monitor.is_alert_condition(self.system_info)
        if alerts:
            lines.extend([f"  {self.unicode_chars['warning']} {alert}" for alert in alerts])
        else:
            lines.append(f"  {self.unicode_chars['check']} No alerts")
        
        self.content_window.set_content(lines)
    
    def _update_process_content(self):
        """Update process content"""
        lines = ["Process List", "=" * 50, ""]
        
        if self.process_list:
            lines.append(f"{'PID':>8} {'USER':>10} {'CPU%':>6} {'MEM%':>6} {'NAME':>15} {'COMMAND'}")
            lines.append("-" * 80)
            
            for proc in self.process_list:
                line = f"{proc.pid:>8} {proc.user:>10} {proc.cpu_percent:>6.1f} {proc.memory_percent:>6.1f} {proc.name:>15} {proc.command[:30]}"
                lines.append(line)
        else:
            lines.append("No process data available")
        
        self.content_window.set_content(lines)
    
    def _update_log_content(self):
        """Update log content"""
        lines = ["System Logs", "=" * 50, ""]
        
        if self.log_lines:
            lines.extend(self.log_lines)
        else:
            lines.append("No log data available")
        
        self.content_window.set_content(lines)
    
    def _update_config_content(self):
        """Update configuration content"""
        lines = [
            "Configuration",
            "=" * 50,
            "",
            "Terminal Configuration:",
            f"  Min Width: {self.config.terminal.min_width}",
            f"  Min Height: {self.config.terminal.min_height}",
            f"  Color Scheme: {self.config.terminal.color_scheme.value}",
            f"  Unicode Support: {self.config.terminal.use_unicode}",
            f"  Mouse Support: {self.config.terminal.mouse_support}",
            f"  Refresh Interval: {self.config.terminal.refresh_interval}s",
            "",
            "System Configuration:",
            f"  Log Directory: {self.config.system.log_directory}",
            f"  Process Scan Interval: {self.config.system.process_scan_interval}s",
            f"  Memory Threshold: {self.config.system.memory_threshold}%",
            f"  CPU Threshold: {self.config.system.cpu_threshold}%",
            f"  Disk Threshold: {self.config.system.disk_threshold}%",
            "",
            "Application:",
            f"  Debug Mode: {self.config.debug_mode}",
            f"  Demo Mode: {self.config.demo_mode}",
        ]
        
        self.content_window.set_content(lines)
    
    def _update_help_content(self):
        """Update help content"""
        lines = [
            "ASP Manager Help",
            "=" * 50,
            "",
            "Navigation:",
            "  Arrow Keys    - Navigate menus and content",
            "  Page Up/Down  - Scroll content",
            "  Home/End      - Go to top/bottom",
            "  Tab           - Switch between panels",
            "",
            "Function Keys:",
            "  F1            - Help",
            "  F2            - Refresh",
            "  F3            - Exit",
            "  F5            - Force refresh",
            "",
            "Menu Options:",
            "  Dashboard     - System overview",
            "  Processes     - Process management",
            "  Logs          - System logs",
            "  Config        - Configuration",
            "  Help          - This help screen",
            "",
            "Status Bar:",
            "  Shows current time, system status, and key bindings",
            "",
            f"Version: {self.config.ui.version}",
            f"Copyright: {self.config.ui.copyright}",
        ]
        
        self.content_window.set_content(lines)
    
    def _handle_input(self, key: int):
        """Handle keyboard input"""
        # Global function keys
        if key == curses.KEY_F1 or key == ord('h'):
            self._switch_screen(MainScreen.HELP)
        elif key == curses.KEY_F2 or key == ord('r'):
            self._force_refresh()
        elif key == curses.KEY_F3 or key == ord('q'):
            self.state = AppState.EXITING
        elif key == 27:  # ESC key
            self.state = AppState.EXITING
        
        # Menu navigation
        elif key == curses.KEY_UP or key == ord('k'):
            if self.menu_window:
                self.menu_window.select_previous()
        elif key == curses.KEY_DOWN or key == ord('j'):
            if self.menu_window:
                self.menu_window.select_next()
        elif key == 10 or key == 13:  # Enter key
            self._handle_menu_selection()
        
        # Content navigation
        elif key == curses.KEY_PPAGE:
            if self.content_window:
                self.content_window.scroll_up(5)
        elif key == curses.KEY_NPAGE:
            if self.content_window:
                self.content_window.scroll_down(5)
        elif key == curses.KEY_HOME:
            if self.content_window:
                self.content_window.scroll_to_top()
        elif key == curses.KEY_END:
            if self.content_window:
                self.content_window.scroll_to_bottom()
    
    def _handle_menu_selection(self):
        """Handle menu selection"""
        if not self.menu_window:
            return
        
        selected_index = self.menu_window.get_selected_index()
        
        if selected_index == 0:
            self._switch_screen(MainScreen.DASHBOARD)
        elif selected_index == 1:
            self._switch_screen(MainScreen.PROCESSES)
        elif selected_index == 2:
            self._switch_screen(MainScreen.LOGS)
        elif selected_index == 3:
            self._switch_screen(MainScreen.CONFIG)
        elif selected_index == 4:
            self._switch_screen(MainScreen.HELP)
    
    def _switch_screen(self, screen: MainScreen):
        """Switch to a different screen"""
        self.current_screen = screen
        self._force_refresh()
    
    def _force_refresh(self):
        """Force a refresh of system data"""
        with self.update_lock:
            self._update_data()
    
    def _signal_handler(self, signum, frame):
        """Handle system signals"""
        self.state = AppState.EXITING
    
    def _cleanup(self):
        """Clean up resources"""
        try:
            # Stop update thread
            if self.update_thread and self.update_thread.is_alive():
                self.state = AppState.EXITING
                self.update_thread.join(timeout=2)
            
            # Clean up UI components
            if self.title_window:
                self.title_window.cleanup()
            if self.menu_window:
                self.menu_window.cleanup()
            if self.content_window:
                self.content_window.cleanup()
            if self.status_window:
                self.status_window.cleanup()
            
            # Restore terminal
            if self.stdscr:
                curses.curs_set(1)
                curses.endwin()
        
        except Exception as e:
            self.logger.error(f"Cleanup error: {e}")


class TitleWindow(BaseWindow):
    """Title window component"""
    
    def __init__(self, parent_window, rect, title, version):
        super().__init__(parent_window, rect, WindowType.MAIN)
        self.title = title
        self.version = version
    
    def draw(self):
        if not self.window:
            return
        
        self.clear()
        
        # Draw title bar
        self.fill_line(0, ' ', 'header')
        self.draw_centered_text(0, f"{self.title} {self.version}", 'header')
        
        # Draw separator
        self.fill_line(1, self.unicode_chars['h_line'], 'border')
        
        # Draw timestamp
        timestamp = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
        self.draw_text(1, 2, timestamp, 'border')
        
        self.refresh()
    
    def handle_input(self, key: int) -> bool:
        return False


class StatusWindow(BaseWindow):
    """Status window component"""
    
    def __init__(self, parent_window, rect):
        super().__init__(parent_window, rect, WindowType.STATUS)
        self.config = get_config()
    
    def draw(self):
        if not self.window:
            return
        
        self.clear()
        
        # Draw separator
        self.fill_line(0, self.unicode_chars['h_line'], 'border')
        
        # Draw help text
        help_text = self.config.ui.help_text
        self.draw_text(1, 2, help_text, 'status')
        
        # Draw status
        status_text = f"Status: Running | {datetime.now().strftime('%H:%M:%S')}"
        self.draw_text(1, self.rect.width - len(status_text) - 2, status_text, 'status')
        
        self.refresh()
    
    def handle_input(self, key: int) -> bool:
        return False


def main():
    """Main entry point"""
    try:
        app = ASPManager()
        app.run()
    except KeyboardInterrupt:
        pass
    except Exception as e:
        print(f"Fatal error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()