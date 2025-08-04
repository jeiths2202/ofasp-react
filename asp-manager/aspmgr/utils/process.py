"""Process management utilities for ASP Manager."""

import psutil
import signal
import subprocess
from typing import Dict, List, Optional
from ..constants import ProcessStatus


class ProcessManager:
    """Manage system processes."""
    
    def __init__(self):
        self.monitored_processes = {}
        
    def get_process_list(self) -> List[Dict]:
        """Get list of all processes."""
        processes = []
        
        for proc in psutil.process_iter(['pid', 'name', 'username', 'cpu_percent', 
                                       'memory_percent', 'status', 'create_time']):
            try:
                info = proc.info
                
                # Get process status
                status = self._map_process_status(info['status'])
                
                processes.append({
                    'pid': info['pid'],
                    'name': info['name'] or 'Unknown',
                    'username': info['username'] or 'Unknown',
                    'cpu': info['cpu_percent'] or 0.0,
                    'memory': info['memory_percent'] or 0.0,
                    'status': status,
                    'start_time': info['create_time']
                })
            except (psutil.NoSuchProcess, psutil.AccessDenied):
                continue
                
        return processes
        
    def get_process_info(self, pid: int) -> Optional[Dict]:
        """Get detailed information about a process."""
        try:
            proc = psutil.Process(pid)
            
            # Get basic info
            info = {
                'pid': pid,
                'name': proc.name(),
                'exe': proc.exe() if proc.exe() else 'Unknown',
                'cmdline': ' '.join(proc.cmdline()) if proc.cmdline() else 'Unknown',
                'username': proc.username(),
                'status': self._map_process_status(proc.status()),
                'create_time': proc.create_time(),
                'num_threads': proc.num_threads(),
                'num_fds': proc.num_fds() if hasattr(proc, 'num_fds') else 0
            }
            
            # Get resource usage
            cpu_times = proc.cpu_times()
            memory_info = proc.memory_info()
            
            info.update({
                'cpu_percent': proc.cpu_percent(interval=0.1),
                'cpu_user': cpu_times.user,
                'cpu_system': cpu_times.system,
                'memory_rss': memory_info.rss,
                'memory_vms': memory_info.vms,
                'memory_percent': proc.memory_percent()
            })
            
            # Get I/O stats if available
            try:
                io_counters = proc.io_counters()
                info.update({
                    'io_read_bytes': io_counters.read_bytes,
                    'io_write_bytes': io_counters.write_bytes,
                    'io_read_count': io_counters.read_count,
                    'io_write_count': io_counters.write_count
                })
            except:
                pass
                
            # Get connections
            try:
                connections = proc.connections()
                info['num_connections'] = len(connections)
            except:
                info['num_connections'] = 0
                
            # Get children
            children = proc.children(recursive=True)
            info['num_children'] = len(children)
            
            return info
            
        except (psutil.NoSuchProcess, psutil.AccessDenied):
            return None
            
    def start_process(self, command: str, args: List[str] = None, 
                     env: Dict[str, str] = None) -> Optional[int]:
        """Start a new process."""
        try:
            cmd = [command]
            if args:
                cmd.extend(args)
                
            proc = subprocess.Popen(
                cmd,
                env=env,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                start_new_session=True
            )
            
            return proc.pid
            
        except Exception as e:
            raise Exception(f"Failed to start process: {e}")
            
    def stop_process(self, pid: int, force: bool = False) -> bool:
        """Stop a process."""
        try:
            proc = psutil.Process(pid)
            
            if force:
                proc.kill()  # SIGKILL
            else:
                proc.terminate()  # SIGTERM
                
            # Wait for process to terminate
            proc.wait(timeout=5)
            return True
            
        except psutil.NoSuchProcess:
            return True  # Already stopped
        except psutil.TimeoutExpired:
            # Force kill if graceful termination failed
            if not force:
                return self.stop_process(pid, force=True)
            return False
        except psutil.AccessDenied:
            raise Exception("Permission denied to stop process")
        except Exception as e:
            raise Exception(f"Failed to stop process: {e}")
            
    def restart_process(self, pid: int) -> Optional[int]:
        """Restart a process."""
        try:
            # Get process info before stopping
            proc = psutil.Process(pid)
            cmdline = proc.cmdline()
            env = proc.environ()
            
            # Stop the process
            if not self.stop_process(pid):
                raise Exception("Failed to stop process")
                
            # Start new instance
            if cmdline:
                return self.start_process(cmdline[0], cmdline[1:], env)
            else:
                raise Exception("Cannot restart: no command line available")
                
        except Exception as e:
            raise Exception(f"Failed to restart process: {e}")
            
    def send_signal(self, pid: int, sig: int) -> bool:
        """Send a signal to a process."""
        try:
            proc = psutil.Process(pid)
            proc.send_signal(sig)
            return True
        except (psutil.NoSuchProcess, psutil.AccessDenied):
            return False
            
    def set_process_priority(self, pid: int, priority: int) -> bool:
        """Set process priority (nice value)."""
        try:
            proc = psutil.Process(pid)
            proc.nice(priority)
            return True
        except (psutil.NoSuchProcess, psutil.AccessDenied):
            return False
            
    def monitor_process(self, pid: int, callback: callable = None):
        """Add a process to monitoring list."""
        self.monitored_processes[pid] = {
            'callback': callback,
            'last_check': None,
            'status': None
        }
        
    def unmonitor_process(self, pid: int):
        """Remove a process from monitoring list."""
        if pid in self.monitored_processes:
            del self.monitored_processes[pid]
            
    def check_monitored_processes(self):
        """Check status of monitored processes."""
        for pid, info in list(self.monitored_processes.items()):
            try:
                proc = psutil.Process(pid)
                status = self._map_process_status(proc.status())
                
                # Check if status changed
                if status != info['status']:
                    info['status'] = status
                    if info['callback']:
                        info['callback'](pid, status)
                        
                info['last_check'] = psutil.time.time()
                
            except psutil.NoSuchProcess:
                # Process no longer exists
                if info['callback']:
                    info['callback'](pid, ProcessStatus.STOPPED.value)
                del self.monitored_processes[pid]
                
    def _map_process_status(self, psutil_status: str) -> str:
        """Map psutil status to our ProcessStatus."""
        status_map = {
            psutil.STATUS_RUNNING: ProcessStatus.RUNNING.value,
            psutil.STATUS_SLEEPING: ProcessStatus.RUNNING.value,
            psutil.STATUS_DISK_SLEEP: ProcessStatus.RUNNING.value,
            psutil.STATUS_STOPPED: ProcessStatus.STOPPED.value,
            psutil.STATUS_ZOMBIE: ProcessStatus.ERROR.value,
            psutil.STATUS_DEAD: ProcessStatus.STOPPED.value
        }
        
        return status_map.get(psutil_status, ProcessStatus.UNKNOWN.value)