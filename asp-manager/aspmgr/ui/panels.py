"""Panel components for ASP Manager CUI application."""

import curses
import time
from typing import Dict, List, Optional, Tuple
from datetime import datetime
from .base import Window, ScrollableWindow
from ..constants import Colors, Symbols, ProcessStatus, Limits


class StatusPanel(Window):
    """Status panel showing system overview."""
    
    def __init__(self, height: int, width: int, y: int, x: int):
        super().__init__(height, width, y, x, "System Status", True, Colors.INFO)
        self.data = {
            'hostname': 'Unknown',
            'uptime': 'Unknown',
            'cpu_usage': 0.0,
            'memory_usage': 0.0,
            'disk_usage': 0.0,
            'active_processes': 0,
            'alerts': []
        }
        
    def update_data(self, data: Dict):
        """Update status data."""
        self.data.update(data)
        self.draw()
        
    def draw(self):
        """Draw the status panel."""
        super().draw()
        
        if not self.visible:
            return
            
        y = 2
        x = 2
        
        # System info
        self.window.addstr(y, x, f"Host: {self.data['hostname']}")
        y += 1
        self.window.addstr(y, x, f"Uptime: {self.data['uptime']}")
        y += 2
        
        # Resource usage
        self.window.attron(curses.A_BOLD)
        self.window.addstr(y, x, "Resource Usage:")
        self.window.attroff(curses.A_BOLD)
        y += 1
        
        # CPU
        cpu_color = self._get_status_color(self.data['cpu_usage'], 
                                          Limits.CPU_WARNING_THRESHOLD,
                                          Limits.CPU_CRITICAL_THRESHOLD)
        self.window.attron(curses.color_pair(cpu_color))
        self._draw_progress_bar(y, x, "CPU", self.data['cpu_usage'], 
                              self.width - 4)
        self.window.attroff(curses.color_pair(cpu_color))
        y += 1
        
        # Memory
        mem_color = self._get_status_color(self.data['memory_usage'],
                                          Limits.MEMORY_WARNING_THRESHOLD,
                                          Limits.MEMORY_CRITICAL_THRESHOLD)
        self.window.attron(curses.color_pair(mem_color))
        self._draw_progress_bar(y, x, "Mem", self.data['memory_usage'],
                              self.width - 4)
        self.window.attroff(curses.color_pair(mem_color))
        y += 1
        
        # Disk
        disk_color = self._get_status_color(self.data['disk_usage'],
                                           Limits.DISK_WARNING_THRESHOLD,
                                           Limits.DISK_CRITICAL_THRESHOLD)
        self.window.attron(curses.color_pair(disk_color))
        self._draw_progress_bar(y, x, "Disk", self.data['disk_usage'],
                              self.width - 4)
        self.window.attroff(curses.color_pair(disk_color))
        y += 2
        
        # Process count
        self.window.addstr(y, x, f"Active Processes: {self.data['active_processes']}")
        y += 2
        
        # Alerts
        if self.data['alerts']:
            self.window.attron(curses.A_BOLD)
            self.window.addstr(y, x, "Alerts:")
            self.window.attroff(curses.A_BOLD)
            y += 1
            
            for alert in self.data['alerts'][:3]:  # Show max 3 alerts
                self.window.attron(curses.color_pair(Colors.STATUS_WARNING))
                self.window.addstr(y, x, f"{Symbols.BULLET} {alert}")
                self.window.attroff(curses.color_pair(Colors.STATUS_WARNING))
                y += 1
                
        self.refresh()
        
    def _draw_progress_bar(self, y: int, x: int, label: str, 
                          value: float, width: int):
        """Draw a progress bar."""
        bar_width = width - len(label) - 8
        filled = int(bar_width * value / 100)
        empty = bar_width - filled
        
        self.window.addstr(y, x, f"{label}: ")
        self.window.addstr(y, x + len(label) + 2, 
                          Symbols.PROGRESS_FULL * filled + 
                          Symbols.PROGRESS_EMPTY * empty)
        self.window.addstr(y, x + width - 5, f"{value:3.0f}%")
        
    def _get_status_color(self, value: float, warning: float, 
                         critical: float) -> int:
        """Get color based on threshold values."""
        if value >= critical:
            return Colors.STATUS_ERROR
        elif value >= warning:
            return Colors.STATUS_WARNING
        else:
            return Colors.STATUS_OK
            

class ProcessPanel(ScrollableWindow):
    """Panel for displaying process list."""
    
    def __init__(self, height: int, width: int, y: int, x: int):
        super().__init__(height, width, y, x, "Process Manager", True, Colors.DEFAULT)
        self.processes = []
        self.sort_column = 'name'
        self.sort_reverse = False
        
    def update_processes(self, processes: List[Dict]):
        """Update process list."""
        self.processes = processes
        self._refresh_display()
        
    def _refresh_display(self):
        """Refresh the process display."""
        # Sort processes
        self.processes.sort(key=lambda p: p.get(self.sort_column, ''),
                           reverse=self.sort_reverse)
        
        # Format process list
        content = []
        header = self._format_header()
        content.append(header)
        content.append('-' * (self.width - 2))
        
        for proc in self.processes:
            line = self._format_process(proc)
            content.append(line)
            
        self.set_content(content)
        self.draw()
        
    def _format_header(self) -> str:
        """Format the header line."""
        # Calculate column widths
        name_width = 20
        pid_width = 8
        status_width = 10
        cpu_width = 8
        mem_width = 8
        
        header = (
            f"{'Name':<{name_width}} "
            f"{'PID':>{pid_width}} "
            f"{'Status':<{status_width}} "
            f"{'CPU%':>{cpu_width}} "
            f"{'Mem%':>{mem_width}}"
        )
        return header
        
    def _format_process(self, proc: Dict) -> str:
        """Format a process line."""
        name_width = 20
        pid_width = 8
        status_width = 10
        cpu_width = 8
        mem_width = 8
        
        # Truncate name if too long
        name = proc.get('name', 'Unknown')
        if len(name) > name_width:
            name = name[:name_width-3] + '...'
            
        line = (
            f"{name:<{name_width}} "
            f"{proc.get('pid', 0):>{pid_width}} "
            f"{proc.get('status', 'Unknown'):<{status_width}} "
            f"{proc.get('cpu', 0.0):>{cpu_width}.1f} "
            f"{proc.get('memory', 0.0):>{mem_width}.1f}"
        )
        return line
        
    def toggle_sort(self, column: str):
        """Toggle sort by column."""
        if self.sort_column == column:
            self.sort_reverse = not self.sort_reverse
        else:
            self.sort_column = column
            self.sort_reverse = False
        self._refresh_display()
        

class LogPanel(ScrollableWindow):
    """Panel for displaying logs."""
    
    def __init__(self, height: int, width: int, y: int, x: int):
        super().__init__(height, width, y, x, "Log Viewer", True, Colors.DEFAULT)
        self.logs = []
        self.filter_level = None
        self.filter_text = ""
        self.follow_mode = True
        
    def add_log(self, timestamp: datetime, level: str, message: str):
        """Add a log entry."""
        self.logs.append({
            'timestamp': timestamp,
            'level': level,
            'message': message
        })
        
        # Limit log size
        if len(self.logs) > Limits.MAX_LOG_LINES:
            self.logs = self.logs[-Limits.MAX_LOG_LINES:]
            
        self._refresh_display()
        
        # Auto-scroll if in follow mode
        if self.follow_mode:
            self.scroll_position = max(0, len(self.content) - self._viewable_height())
            
    def set_filter(self, level: Optional[str] = None, text: str = ""):
        """Set log filters."""
        self.filter_level = level
        self.filter_text = text.lower()
        self._refresh_display()
        
    def toggle_follow(self):
        """Toggle follow mode."""
        self.follow_mode = not self.follow_mode
        
    def _refresh_display(self):
        """Refresh the log display."""
        content = []
        
        for log in self.logs:
            # Apply filters
            if self.filter_level and log['level'] != self.filter_level:
                continue
            if self.filter_text and self.filter_text not in log['message'].lower():
                continue
                
            # Format log line
            line = self._format_log(log)
            content.append(line)
            
        self.set_content(content)
        self.draw()
        
    def _format_log(self, log: Dict) -> str:
        """Format a log entry."""
        timestamp = log['timestamp'].strftime('%H:%M:%S')
        level = log['level'][:5].ljust(5)
        message = log['message']
        
        # Truncate message if needed
        max_msg_len = self.width - 18
        if len(message) > max_msg_len:
            message = message[:max_msg_len-3] + '...'
            
        return f"{timestamp} [{level}] {message}"
        
    def draw(self):
        """Draw the log panel."""
        super().draw()
        
        if not self.visible:
            return
            
        # Draw status line
        status = "Follow: " + ("ON" if self.follow_mode else "OFF")
        if self.filter_level:
            status += f" | Level: {self.filter_level}"
        if self.filter_text:
            status += f" | Filter: {self.filter_text}"
            
        y = self.height - 1
        x = 2
        self.window.attron(curses.A_DIM)
        self.window.addstr(y, x, status)
        self.window.attroff(curses.A_DIM)
        
        self.refresh()
        

class MetricsPanel(Window):
    """Panel for displaying performance metrics."""
    
    def __init__(self, height: int, width: int, y: int, x: int):
        super().__init__(height, width, y, x, "Performance Metrics", True, Colors.INFO)
        self.metrics_history = {
            'cpu': [],
            'memory': [],
            'disk_io': [],
            'network': []
        }
        self.max_history = 60  # Keep 60 data points
        
    def update_metrics(self, metrics: Dict):
        """Update metrics data."""
        # Add to history
        for key in ['cpu', 'memory', 'disk_io', 'network']:
            if key in metrics:
                self.metrics_history[key].append(metrics[key])
                # Limit history size
                if len(self.metrics_history[key]) > self.max_history:
                    self.metrics_history[key].pop(0)
                    
        self.draw()
        
    def draw(self):
        """Draw the metrics panel."""
        super().draw()
        
        if not self.visible:
            return
            
        y = 2
        x = 2
        
        # Draw each metric graph
        graph_height = 4
        graph_width = self.width - 4
        
        for metric_name in ['cpu', 'memory', 'disk_io', 'network']:
            history = self.metrics_history[metric_name]
            if history:
                self._draw_mini_graph(y, x, metric_name.upper(), 
                                    history, graph_width, graph_height)
            y += graph_height + 1
            
        self.refresh()
        
    def _draw_mini_graph(self, y: int, x: int, label: str, 
                        data: List[float], width: int, height: int):
        """Draw a mini graph for metrics."""
        # Label
        self.window.attron(curses.A_BOLD)
        self.window.addstr(y, x, label)
        self.window.attroff(curses.A_BOLD)
        
        # Current value
        if data:
            current = data[-1]
            self.window.addstr(y, x + width - 10, f"{current:6.1f}%")
            
        # Graph area
        graph_width = width - 12
        y += 1
        
        # Draw graph
        if len(data) >= 2:
            # Normalize data to fit in height
            max_val = max(data[-graph_width:]) if data else 100
            min_val = min(data[-graph_width:]) if data else 0
            range_val = max_val - min_val or 1
            
            for row in range(height - 1):
                line = ""
                threshold = min_val + (range_val * (height - row - 2) / (height - 2))
                
                for i in range(min(len(data), graph_width)):
                    idx = len(data) - graph_width + i
                    if idx >= 0 and data[idx] >= threshold:
                        line += Symbols.PROGRESS_FULL
                    else:
                        line += " "
                        
                self.window.addstr(y + row, x + 2, line)
                
        # Draw axis
        self.window.addstr(y + height - 1, x + 1, "└" + "─" * graph_width)