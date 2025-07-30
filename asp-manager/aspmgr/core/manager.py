"""Core manager for ASP Manager CUI application."""

import curses
import threading
import time
from typing import Dict, List, Optional, Callable
from datetime import datetime
from ..config import config
from ..constants import Colors, Keys, MenuItems, Messages
from ..ui.base import MenuWindow
from ..ui.dialogs import MessageDialog, ConfirmDialog, InputDialog, ListDialog
from ..ui.panels import StatusPanel, ProcessPanel, LogPanel, MetricsPanel
from ..utils.system import SystemMonitor
from ..utils.process import ProcessManager
from ..utils.logger import Logger


class AspManager:
    """Main application manager."""
    
    def __init__(self, stdscr):
        self.stdscr = stdscr
        self.running = True
        self.current_panel = "status"
        self.panels = {}
        self.system_monitor = SystemMonitor()
        self.process_manager = ProcessManager()
        self.logger = Logger()
        
        # Initialize curses
        self._init_curses()
        
        # Check terminal size
        self.height, self.width = self.stdscr.getmaxyx()
        if self.height < config.ui.min_height or self.width < config.ui.min_width:
            raise ValueError(f"Terminal too small. Minimum size: "
                           f"{config.ui.min_width}x{config.ui.min_height}")
        
        # Initialize UI components
        self._init_ui()
        
        # Start background threads
        self._start_background_tasks()
        
    def _init_curses(self):
        """Initialize curses settings."""
        # Clear screen
        self.stdscr.clear()
        
        # Initialize colors
        curses.start_color()
        curses.use_default_colors()
        
        # Define color pairs
        curses.init_pair(Colors.DEFAULT, curses.COLOR_WHITE, -1)
        curses.init_pair(Colors.HEADER, curses.COLOR_WHITE, curses.COLOR_BLUE)
        curses.init_pair(Colors.FOOTER, curses.COLOR_WHITE, curses.COLOR_BLUE)
        curses.init_pair(Colors.MENU, curses.COLOR_WHITE, -1)
        curses.init_pair(Colors.MENU_SELECTED, curses.COLOR_BLACK, curses.COLOR_CYAN)
        curses.init_pair(Colors.STATUS_OK, curses.COLOR_GREEN, -1)
        curses.init_pair(Colors.STATUS_WARNING, curses.COLOR_YELLOW, -1)
        curses.init_pair(Colors.STATUS_ERROR, curses.COLOR_RED, -1)
        curses.init_pair(Colors.INFO, curses.COLOR_CYAN, -1)
        curses.init_pair(Colors.HIGHLIGHT, curses.COLOR_WHITE, curses.COLOR_BLUE)
        curses.init_pair(Colors.BORDER, curses.COLOR_BLUE, -1)
        curses.init_pair(Colors.DIALOG, curses.COLOR_WHITE, curses.COLOR_BLACK)
        curses.init_pair(Colors.INPUT, curses.COLOR_WHITE, curses.COLOR_BLACK)
        curses.init_pair(Colors.BUTTON, curses.COLOR_WHITE, curses.COLOR_BLUE)
        curses.init_pair(Colors.BUTTON_SELECTED, curses.COLOR_YELLOW, curses.COLOR_BLUE)
        
        # Hide cursor
        curses.curs_set(0)
        
        # Enable keypad
        self.stdscr.keypad(True)
        
        # Enable mouse if configured
        if config.ui.enable_mouse:
            curses.mousemask(curses.ALL_MOUSE_EVENTS)
            
        # Non-blocking input
        self.stdscr.nodelay(True)
        
    def _init_ui(self):
        """Initialize UI components."""
        # Calculate layout
        sidebar_width = int(self.width * config.ui.sidebar_width_percent / 100)
        main_width = self.width - sidebar_width
        content_height = self.height - config.ui.header_height - config.ui.footer_height
        
        # Create header
        self._draw_header()
        
        # Create menu
        menu_items = [
            (MenuItems.SYSTEM_STATUS.value, lambda: self._switch_panel("status")),
            (MenuItems.PROCESS_MANAGER.value, lambda: self._switch_panel("process")),
            (MenuItems.CONFIGURATION.value, lambda: self._switch_panel("config")),
            (MenuItems.LOG_VIEWER.value, lambda: self._switch_panel("logs")),
            (MenuItems.PERFORMANCE.value, lambda: self._switch_panel("metrics")),
            (MenuItems.TOOLS.value, lambda: self._show_tools_menu()),
            (MenuItems.HELP.value, lambda: self._show_help()),
            (MenuItems.EXIT.value, lambda: self._confirm_exit())
        ]
        
        self.menu = MenuWindow(
            content_height, sidebar_width,
            config.ui.header_height, 0,
            "Main Menu", menu_items
        )
        
        # Create panels
        panel_x = sidebar_width
        panel_y = config.ui.header_height
        panel_width = main_width
        panel_height = content_height
        
        self.panels['status'] = StatusPanel(panel_height, panel_width, panel_y, panel_x)
        self.panels['process'] = ProcessPanel(panel_height, panel_width, panel_y, panel_x)
        self.panels['logs'] = LogPanel(panel_height, panel_width, panel_y, panel_x)
        self.panels['metrics'] = MetricsPanel(panel_height, panel_width, panel_y, panel_x)
        
        # Show initial panel
        self._switch_panel("status")
        
        # Create footer
        self._draw_footer()
        
        # Draw everything
        self.menu.draw()
        self.stdscr.refresh()
        
    def _draw_header(self):
        """Draw the header."""
        header_text = f" {config.app_name} v{config.version} "
        
        # Draw header background
        self.stdscr.attron(curses.color_pair(Colors.HEADER))
        for y in range(config.ui.header_height - 1):
            self.stdscr.addstr(y, 0, " " * self.width)
            
        # Draw title
        x = (self.width - len(header_text)) // 2
        self.stdscr.addstr(1, x, header_text, curses.A_BOLD)
        
        # Draw user info
        user_info = f"User: {self.system_monitor.get_current_user()}"
        self.stdscr.addstr(1, self.width - len(user_info) - 2, user_info)
        
        # Draw separator
        self.stdscr.addstr(config.ui.header_height - 1, 0, "─" * self.width)
        self.stdscr.attroff(curses.color_pair(Colors.HEADER))
        
    def _draw_footer(self):
        """Draw the footer."""
        y = self.height - config.ui.footer_height
        
        # Draw separator
        self.stdscr.attron(curses.color_pair(Colors.FOOTER))
        self.stdscr.addstr(y, 0, "─" * self.width)
        
        # Draw footer background
        for i in range(1, config.ui.footer_height):
            self.stdscr.addstr(y + i, 0, " " * self.width)
            
        # Draw function key hints
        hints = [
            "F1:Help", "F2:Refresh", "F5:Tools", "F10:Exit",
            "Tab:Switch", "Enter:Select"
        ]
        hint_text = "  ".join(hints)
        x = (self.width - len(hint_text)) // 2
        self.stdscr.addstr(y + 1, x, hint_text)
        
        # Draw clock
        self._update_clock()
        
        self.stdscr.attroff(curses.color_pair(Colors.FOOTER))
        
    def _update_clock(self):
        """Update the clock in footer."""
        current_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        y = self.height - 1
        x = self.width - len(current_time) - 2
        
        self.stdscr.attron(curses.color_pair(Colors.FOOTER))
        self.stdscr.addstr(y, x, current_time)
        self.stdscr.attroff(curses.color_pair(Colors.FOOTER))
        
    def _switch_panel(self, panel_name: str):
        """Switch to a different panel."""
        # Hide current panel
        if self.current_panel in self.panels:
            self.panels[self.current_panel].hide()
            
        # Show new panel
        self.current_panel = panel_name
        if panel_name in self.panels:
            self.panels[panel_name].show()
            
    def _start_background_tasks(self):
        """Start background monitoring tasks."""
        # Status update thread
        def update_status():
            while self.running:
                data = self.system_monitor.get_system_status()
                if 'status' in self.panels:
                    self.panels['status'].update_data(data)
                time.sleep(config.ui.status_refresh_interval / 1000)
                
        # Process update thread
        def update_processes():
            while self.running:
                processes = self.process_manager.get_process_list()
                if 'process' in self.panels:
                    self.panels['process'].update_processes(processes)
                time.sleep(config.system.process_check_interval)
                
        # Metrics update thread
        def update_metrics():
            while self.running:
                metrics = self.system_monitor.get_performance_metrics()
                if 'metrics' in self.panels:
                    self.panels['metrics'].update_metrics(metrics)
                time.sleep(config.ui.metrics_refresh_interval / 1000)
                
        # Start threads
        threading.Thread(target=update_status, daemon=True).start()
        threading.Thread(target=update_processes, daemon=True).start()
        threading.Thread(target=update_metrics, daemon=True).start()
        
    def _show_tools_menu(self):
        """Show tools submenu."""
        tools = [
            "System Information",
            "Network Status",
            "Service Manager",
            "File Browser",
            "Terminal"
        ]
        
        dialog = ListDialog("Tools", "Select a tool:", tools,
                          self.height, self.width)
        result = dialog.show_and_wait()
        
        if result:
            self._show_message("Tool Selected", f"You selected: {result}")
            
    def _show_help(self):
        """Show help dialog."""
        help_text = """
ASP Manager - System Management Tool

Navigation:
- Use arrow keys to navigate menus
- Press Enter to select
- Press Tab to switch between panels
- Press ESC to go back

Function Keys:
- F1: Show this help
- F2: Refresh current view
- F5: Open tools menu
- F10: Exit application

For more information, see the documentation.
"""
        dialog = MessageDialog("Help", help_text.strip(), 
                             self.height, self.width)
        dialog.show_and_wait()
        
    def _confirm_exit(self):
        """Confirm exit."""
        dialog = ConfirmDialog("Exit", Messages.CONFIRM_EXIT,
                             self.height, self.width)
        if dialog.show_and_wait():
            self.running = False
            
    def _show_message(self, title: str, message: str):
        """Show a message dialog."""
        dialog = MessageDialog(title, message, self.height, self.width)
        dialog.show_and_wait()
        
    def handle_resize(self):
        """Handle terminal resize."""
        self.height, self.width = self.stdscr.getmaxyx()
        
        # Redraw everything
        self.stdscr.clear()
        self._draw_header()
        self._draw_footer()
        
        # Resize panels
        sidebar_width = int(self.width * config.ui.sidebar_width_percent / 100)
        main_width = self.width - sidebar_width
        content_height = self.height - config.ui.header_height - config.ui.footer_height
        
        self.menu.resize(content_height, sidebar_width)
        self.menu.move(config.ui.header_height, 0)
        
        for panel in self.panels.values():
            panel.resize(content_height, main_width)
            panel.move(config.ui.header_height, sidebar_width)
            
        # Redraw current panel
        self.menu.draw()
        if self.current_panel in self.panels:
            self.panels[self.current_panel].draw()
            
        self.stdscr.refresh()
        
    def run(self):
        """Main application loop."""
        last_resize_check = time.time()
        
        while self.running:
            try:
                # Check for resize
                if time.time() - last_resize_check > 0.5:
                    new_height, new_width = self.stdscr.getmaxyx()
                    if new_height != self.height or new_width != self.width:
                        self.handle_resize()
                    last_resize_check = time.time()
                    
                # Update clock
                self._update_clock()
                
                # Get input
                key = self.stdscr.getch()
                if key == -1:
                    time.sleep(0.05)
                    continue
                    
                # Handle global keys
                if key == Keys.F10:
                    self._confirm_exit()
                elif key == Keys.F1:
                    self._show_help()
                elif key == Keys.F2:
                    if self.current_panel in self.panels:
                        self.panels[self.current_panel].draw()
                elif key == Keys.F5:
                    self._show_tools_menu()
                elif key == Keys.TAB:
                    # Switch focus between menu and panel
                    pass
                else:
                    # Pass to menu
                    self.menu.handle_key(key)
                    
                self.stdscr.refresh()
                
            except KeyboardInterrupt:
                self._confirm_exit()
            except Exception as e:
                self.logger.error(f"Error in main loop: {e}")
                self._show_message("Error", str(e))