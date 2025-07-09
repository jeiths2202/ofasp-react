#!/usr/bin/env python3
"""
ASP Manager System Monitoring Module
Provides system monitoring capabilities without external dependencies
"""

import os
import time
import subprocess
import threading
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass
from datetime import datetime

from .config import get_config


@dataclass
class SystemInfo:
    """System information data structure"""
    hostname: str
    uptime: float
    load_avg: Tuple[float, float, float]
    cpu_percent: float
    memory_total: int
    memory_used: int
    memory_percent: float
    disk_total: int
    disk_used: int
    disk_percent: float
    process_count: int
    timestamp: datetime


@dataclass
class ProcessInfo:
    """Process information data structure"""
    pid: int
    name: str
    user: str
    cpu_percent: float
    memory_percent: float
    memory_rss: int
    status: str
    command: str
    start_time: str


class SystemMonitor:
    """System monitoring class using system commands"""
    
    def __init__(self):
        self.config = get_config()
        self.last_cpu_times = None
        self.last_measurement_time = None
        self._lock = threading.Lock()
    
    def get_system_info(self) -> SystemInfo:
        """Get comprehensive system information"""
        with self._lock:
            try:
                hostname = self._get_hostname()
                uptime = self._get_uptime()
                load_avg = self._get_load_average()
                cpu_percent = self._get_cpu_percent()
                memory_info = self._get_memory_info()
                disk_info = self._get_disk_info()
                process_count = self._get_process_count()
                
                return SystemInfo(
                    hostname=hostname,
                    uptime=uptime,
                    load_avg=load_avg,
                    cpu_percent=cpu_percent,
                    memory_total=memory_info['total'],
                    memory_used=memory_info['used'],
                    memory_percent=memory_info['percent'],
                    disk_total=disk_info['total'],
                    disk_used=disk_info['used'],
                    disk_percent=disk_info['percent'],
                    process_count=process_count,
                    timestamp=datetime.now()
                )
            except Exception as e:
                # Return demo data if system monitoring fails
                return self._get_demo_system_info()
    
    def get_process_list(self) -> List[ProcessInfo]:
        """Get list of running processes"""
        try:
            return self._get_process_list_from_ps()
        except Exception:
            return self._get_demo_process_list()
    
    def get_top_processes(self, limit: int = 10, sort_by: str = 'cpu') -> List[ProcessInfo]:
        """Get top processes sorted by CPU or memory usage"""
        processes = self.get_process_list()
        
        if sort_by == 'cpu':
            processes.sort(key=lambda p: p.cpu_percent, reverse=True)
        elif sort_by == 'memory':
            processes.sort(key=lambda p: p.memory_percent, reverse=True)
        
        return processes[:limit]
    
    def _get_hostname(self) -> str:
        """Get system hostname"""
        try:
            result = subprocess.run(['hostname'], capture_output=True, text=True, timeout=5)
            return result.stdout.strip() if result.returncode == 0 else 'unknown'
        except:
            return 'localhost'
    
    def _get_uptime(self) -> float:
        """Get system uptime in seconds"""
        try:
            with open('/proc/uptime', 'r') as f:
                return float(f.read().split()[0])
        except:
            return 0.0
    
    def _get_load_average(self) -> Tuple[float, float, float]:
        """Get system load averages"""
        try:
            with open('/proc/loadavg', 'r') as f:
                loads = f.read().split()
                return (float(loads[0]), float(loads[1]), float(loads[2]))
        except:
            return (0.0, 0.0, 0.0)
    
    def _get_cpu_percent(self) -> float:
        """Get CPU usage percentage"""
        try:
            # Read CPU stats from /proc/stat
            with open('/proc/stat', 'r') as f:
                line = f.readline()
                cpu_times = [int(x) for x in line.split()[1:]]
            
            current_time = time.time()
            
            if self.last_cpu_times and self.last_measurement_time:
                time_delta = current_time - self.last_measurement_time
                if time_delta > 0:
                    # Calculate deltas
                    deltas = [curr - last for curr, last in zip(cpu_times, self.last_cpu_times)]
                    total_delta = sum(deltas)
                    
                    if total_delta > 0:
                        # CPU usage = (total - idle) / total * 100
                        idle_delta = deltas[3] if len(deltas) > 3 else 0
                        cpu_percent = (total_delta - idle_delta) / total_delta * 100
                        
                        self.last_cpu_times = cpu_times
                        self.last_measurement_time = current_time
                        return max(0, min(100, cpu_percent))
            
            # First run or error - initialize
            self.last_cpu_times = cpu_times
            self.last_measurement_time = current_time
            return 0.0
            
        except:
            return 0.0
    
    def _get_memory_info(self) -> Dict[str, int]:
        """Get memory usage information"""
        try:
            with open('/proc/meminfo', 'r') as f:
                meminfo = {}
                for line in f:
                    key, value = line.split(':')
                    meminfo[key] = int(value.split()[0]) * 1024  # Convert to bytes
                
                total = meminfo.get('MemTotal', 0)
                available = meminfo.get('MemAvailable', meminfo.get('MemFree', 0))
                used = total - available
                percent = (used / total * 100) if total > 0 else 0
                
                return {
                    'total': total,
                    'used': used,
                    'percent': percent
                }
        except:
            return {'total': 0, 'used': 0, 'percent': 0}
    
    def _get_disk_info(self) -> Dict[str, int]:
        """Get disk usage information"""
        try:
            statvfs = os.statvfs('/')
            total = statvfs.f_frsize * statvfs.f_blocks
            available = statvfs.f_frsize * statvfs.f_available
            used = total - available
            percent = (used / total * 100) if total > 0 else 0
            
            return {
                'total': total,
                'used': used,
                'percent': percent
            }
        except:
            return {'total': 0, 'used': 0, 'percent': 0}
    
    def _get_process_count(self) -> int:
        """Get total number of processes"""
        try:
            proc_dirs = [d for d in os.listdir('/proc') if d.isdigit()]
            return len(proc_dirs)
        except:
            return 0
    
    def _get_process_list_from_ps(self) -> List[ProcessInfo]:
        """Get process list using ps command"""
        try:
            # Use ps command to get process information
            cmd = ['ps', 'aux', '--no-headers']
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=10)
            
            if result.returncode != 0:
                return []
            
            processes = []
            for line in result.stdout.strip().split('\n'):
                if not line:
                    continue
                
                parts = line.split(None, 10)
                if len(parts) >= 11:
                    try:
                        process = ProcessInfo(
                            pid=int(parts[1]),
                            name=parts[10].split()[0] if parts[10] else 'unknown',
                            user=parts[0],
                            cpu_percent=float(parts[2]),
                            memory_percent=float(parts[3]),
                            memory_rss=int(parts[5]) * 1024,  # Convert to bytes
                            status=parts[7],
                            command=parts[10],
                            start_time=parts[8]
                        )
                        processes.append(process)
                    except (ValueError, IndexError):
                        continue
            
            return processes
            
        except Exception:
            return []
    
    def _get_demo_system_info(self) -> SystemInfo:
        """Get demo system information when real monitoring fails"""
        return SystemInfo(
            hostname='demo-host',
            uptime=86400.0,  # 1 day
            load_avg=(0.5, 0.7, 0.9),
            cpu_percent=25.0,
            memory_total=8 * 1024 * 1024 * 1024,  # 8GB
            memory_used=2 * 1024 * 1024 * 1024,   # 2GB
            memory_percent=25.0,
            disk_total=100 * 1024 * 1024 * 1024,  # 100GB
            disk_used=30 * 1024 * 1024 * 1024,    # 30GB
            disk_percent=30.0,
            process_count=125,
            timestamp=datetime.now()
        )
    
    def _get_demo_process_list(self) -> List[ProcessInfo]:
        """Get demo process list when real monitoring fails"""
        demo_processes = [
            ProcessInfo(1, 'systemd', 'root', 0.1, 0.2, 1024*1024, 'S', '/sbin/init', '00:00'),
            ProcessInfo(2, 'kthreadd', 'root', 0.0, 0.0, 0, 'S', '[kthreadd]', '00:00'),
            ProcessInfo(100, 'python3', 'user', 15.5, 2.1, 50*1024*1024, 'R', 'python3 aspmgr.py', '10:30'),
            ProcessInfo(200, 'bash', 'user', 0.5, 0.8, 5*1024*1024, 'S', '-bash', '09:00'),
            ProcessInfo(300, 'ssh', 'user', 0.2, 0.3, 3*1024*1024, 'S', 'ssh server', '11:15'),
        ]
        return demo_processes
    
    def format_bytes(self, bytes_value: int) -> str:
        """Format bytes into human readable format"""
        for unit in ['B', 'KB', 'MB', 'GB', 'TB']:
            if bytes_value < 1024.0:
                return f"{bytes_value:.1f} {unit}"
            bytes_value /= 1024.0
        return f"{bytes_value:.1f} PB"
    
    def format_uptime(self, seconds: float) -> str:
        """Format uptime into human readable format"""
        days = int(seconds // 86400)
        hours = int((seconds % 86400) // 3600)
        minutes = int((seconds % 3600) // 60)
        
        if days > 0:
            return f"{days}d {hours}h {minutes}m"
        elif hours > 0:
            return f"{hours}h {minutes}m"
        else:
            return f"{minutes}m"
    
    def is_alert_condition(self, system_info: SystemInfo) -> List[str]:
        """Check for alert conditions"""
        alerts = []
        
        if system_info.cpu_percent > self.config.system.cpu_threshold:
            alerts.append(f"High CPU usage: {system_info.cpu_percent:.1f}%")
        
        if system_info.memory_percent > self.config.system.memory_threshold:
            alerts.append(f"High memory usage: {system_info.memory_percent:.1f}%")
        
        if system_info.disk_percent > self.config.system.disk_threshold:
            alerts.append(f"High disk usage: {system_info.disk_percent:.1f}%")
        
        return alerts


# Global system monitor instance
_system_monitor = None


def get_system_monitor() -> SystemMonitor:
    """Get the global system monitor instance"""
    global _system_monitor
    if _system_monitor is None:
        _system_monitor = SystemMonitor()
    return _system_monitor