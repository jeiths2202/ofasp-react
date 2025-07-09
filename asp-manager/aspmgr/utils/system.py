"""System monitoring utilities for ASP Manager."""

import os
import psutil
import socket
import pwd
from typing import Dict, List
from datetime import datetime, timedelta


class SystemMonitor:
    """Monitor system resources and status."""
    
    def __init__(self):
        self.hostname = socket.gethostname()
        self.boot_time = datetime.fromtimestamp(psutil.boot_time())
        
    def get_system_status(self) -> Dict:
        """Get current system status."""
        # Calculate uptime
        uptime = datetime.now() - self.boot_time
        uptime_str = self._format_uptime(uptime)
        
        # Get resource usage
        cpu_percent = psutil.cpu_percent(interval=0.1)
        memory = psutil.virtual_memory()
        disk = psutil.disk_usage('/')
        
        # Count processes
        process_count = len(psutil.pids())
        
        # Check for alerts
        alerts = []
        if cpu_percent > 80:
            alerts.append(f"High CPU usage: {cpu_percent:.1f}%")
        if memory.percent > 85:
            alerts.append(f"High memory usage: {memory.percent:.1f}%")
        if disk.percent > 90:
            alerts.append(f"Low disk space: {disk.percent:.1f}% used")
            
        return {
            'hostname': self.hostname,
            'uptime': uptime_str,
            'cpu_usage': cpu_percent,
            'memory_usage': memory.percent,
            'disk_usage': disk.percent,
            'active_processes': process_count,
            'alerts': alerts
        }
        
    def get_performance_metrics(self) -> Dict:
        """Get detailed performance metrics."""
        # CPU metrics
        cpu_percent = psutil.cpu_percent(interval=0.1)
        cpu_freq = psutil.cpu_freq()
        
        # Memory metrics
        memory = psutil.virtual_memory()
        swap = psutil.swap_memory()
        
        # Disk I/O
        disk_io = psutil.disk_io_counters()
        disk_read_rate = 0
        disk_write_rate = 0
        
        if hasattr(self, '_last_disk_io'):
            time_delta = 1  # Approximate
            disk_read_rate = (disk_io.read_bytes - self._last_disk_io.read_bytes) / time_delta / 1024 / 1024
            disk_write_rate = (disk_io.write_bytes - self._last_disk_io.write_bytes) / time_delta / 1024 / 1024
            
        self._last_disk_io = disk_io
        
        # Network I/O
        net_io = psutil.net_io_counters()
        net_sent_rate = 0
        net_recv_rate = 0
        
        if hasattr(self, '_last_net_io'):
            time_delta = 1  # Approximate
            net_sent_rate = (net_io.bytes_sent - self._last_net_io.bytes_sent) / time_delta / 1024 / 1024
            net_recv_rate = (net_io.bytes_recv - self._last_net_io.bytes_recv) / time_delta / 1024 / 1024
            
        self._last_net_io = net_io
        
        return {
            'cpu': cpu_percent,
            'cpu_freq': cpu_freq.current if cpu_freq else 0,
            'memory': memory.percent,
            'swap': swap.percent,
            'disk_io': max(disk_read_rate, disk_write_rate),
            'disk_read': disk_read_rate,
            'disk_write': disk_write_rate,
            'network': max(net_sent_rate, net_recv_rate),
            'net_sent': net_sent_rate,
            'net_recv': net_recv_rate
        }
        
    def get_current_user(self) -> str:
        """Get current user name."""
        try:
            return pwd.getpwuid(os.getuid()).pw_name
        except:
            return os.getenv('USER', 'unknown')
            
    def get_system_info(self) -> Dict:
        """Get detailed system information."""
        # OS info
        import platform
        uname = platform.uname()
        
        # CPU info
        cpu_count = psutil.cpu_count()
        cpu_count_logical = psutil.cpu_count(logical=True)
        
        # Memory info
        memory = psutil.virtual_memory()
        swap = psutil.swap_memory()
        
        # Disk info
        partitions = []
        for partition in psutil.disk_partitions():
            try:
                usage = psutil.disk_usage(partition.mountpoint)
                partitions.append({
                    'device': partition.device,
                    'mountpoint': partition.mountpoint,
                    'fstype': partition.fstype,
                    'total': self._format_bytes(usage.total),
                    'used': self._format_bytes(usage.used),
                    'free': self._format_bytes(usage.free),
                    'percent': usage.percent
                })
            except:
                pass
                
        # Network interfaces
        interfaces = []
        for name, addrs in psutil.net_if_addrs().items():
            for addr in addrs:
                if addr.family == socket.AF_INET:
                    interfaces.append({
                        'name': name,
                        'ip': addr.address,
                        'netmask': addr.netmask
                    })
                    
        return {
            'os': {
                'system': uname.system,
                'node': uname.node,
                'release': uname.release,
                'version': uname.version,
                'machine': uname.machine
            },
            'cpu': {
                'physical_cores': cpu_count,
                'logical_cores': cpu_count_logical,
                'model': self._get_cpu_model()
            },
            'memory': {
                'total': self._format_bytes(memory.total),
                'available': self._format_bytes(memory.available),
                'used': self._format_bytes(memory.used),
                'percent': memory.percent
            },
            'swap': {
                'total': self._format_bytes(swap.total),
                'used': self._format_bytes(swap.used),
                'free': self._format_bytes(swap.free),
                'percent': swap.percent
            },
            'disks': partitions,
            'network': interfaces
        }
        
    def _format_uptime(self, uptime: timedelta) -> str:
        """Format uptime for display."""
        days = uptime.days
        hours, remainder = divmod(uptime.seconds, 3600)
        minutes, _ = divmod(remainder, 60)
        
        parts = []
        if days > 0:
            parts.append(f"{days}d")
        if hours > 0:
            parts.append(f"{hours}h")
        if minutes > 0:
            parts.append(f"{minutes}m")
            
        return " ".join(parts) if parts else "0m"
        
    def _format_bytes(self, bytes_val: int) -> str:
        """Format bytes to human readable format."""
        for unit in ['B', 'KB', 'MB', 'GB', 'TB']:
            if bytes_val < 1024.0:
                return f"{bytes_val:.2f} {unit}"
            bytes_val /= 1024.0
        return f"{bytes_val:.2f} PB"
        
    def _get_cpu_model(self) -> str:
        """Get CPU model name."""
        try:
            with open('/proc/cpuinfo', 'r') as f:
                for line in f:
                    if line.startswith('model name'):
                        return line.split(':')[1].strip()
        except:
            pass
        return "Unknown"