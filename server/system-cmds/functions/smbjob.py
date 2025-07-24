# -*- coding: utf-8 -*-
"""
SMBJOB (Submit Job) Command Implementation for OpenASP

Based on Fujitsu ASP SMBJOB command specifications from the manual.
Provides job submission and background execution capabilities.
"""

import os
import sys
import json
import threading
import queue
import subprocess
import uuid
import time
from datetime import datetime
from typing import Dict, List, Tuple, Optional, Any

# Import from parent module
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from asp_commands import VOLUME_ROOT, get_catalog_info, set_pgmec, reset_pgmec

# Global job management
JOB_QUEUE = queue.PriorityQueue()
ACTIVE_JOBS = {}
JOB_LOG_DIR = os.path.join(VOLUME_ROOT, "JOBLOG")

# Ensure job log directory exists
os.makedirs(JOB_LOG_DIR, exist_ok=True)

class JobInfo:
    """Job information class for SMBJOB"""
    
    def __init__(self, job_id: str, job_name: str, program: str, library: str = None, 
                 volume: str = "DISK01", parameters: str = "", priority: int = 5,
                 jobq: str = "@SAME", jobk: str = "@SAME", hold: bool = False):
        self.job_id = job_id
        self.job_name = job_name
        self.program = program
        self.library = library
        self.volume = volume
        self.parameters = parameters
        self.priority = priority
        self.jobq = jobq
        self.jobk = jobk
        self.hold = hold
        self.status = "PENDING"  # PENDING, RUNNING, COMPLETED, ERROR, HELD
        self.submitted_time = datetime.now()
        self.start_time = None
        self.end_time = None
        self.return_code = None
        self.log_file = os.path.join(JOB_LOG_DIR, f"{job_id}_{job_name}.log")

def SMBJOB(command: str) -> bool:
    """
    SMBJOB command - Submit Job
    
    Fujitsu ASP Format: SMBJOB JOB=jobname,PARA=parameters,JOBQ=queue,@LIB=library[,VOL=volume]
    Enhanced format: SMBJOB JOB=jobname,PGM=program[,PARA=parameters][,JOBQ=queue][,JOBK=keyword][,HLDJOB=hold]
    
    Args:
        command: Full SMBJOB command string
        
    Returns:
        True if job submitted successfully, False otherwise
    """
    try:
        reset_pgmec()
        
        # Parse command with support for both formats
        command_str = command.replace('SMBJOB ', '').strip()
        
        # Initialize default values based on Fujitsu ASP manual
        params = {
            'JOB': '',
            'PGM': '',
            'PARA': '',
            'LIB': '',
            'VOL': 'DISK01',
            'JOBQ': '@SAME',
            'JOBK': '@SAME', 
            'HLDJOB': '@NO',
            'JOBPTYY': '5',
            'MEMPTYY': '@SAME',
            'VSIZE': '@SAME',
            'EVSIZE': '@SAME'
        }
        
        # Parse parameters
        parts = []
        current_part = ""
        in_parentheses = False
        
        for char in command_str:
            if char == '(':
                in_parentheses = True
                current_part += char
            elif char == ')':
                in_parentheses = False
                current_part += char
            elif char == ',' and not in_parentheses:
                if current_part.strip():
                    parts.append(current_part.strip())
                current_part = ""
            else:
                current_part += char
        
        if current_part.strip():
            parts.append(current_part.strip())
        
        # Process each parameter
        for part in parts:
            if '=' in part:
                key, value = part.split('=', 1)
                key = key.strip()
                value = value.strip()
                
                # Handle @ prefixed parameters (like @LIB=value)
                if key.startswith('@'):
                    key = key[1:].upper()  # Remove @ and normalize
                else:
                    key = key.upper()
                
                if key in params:
                    params[key] = value
        
        # Validate required parameters
        if not params['JOB']:
            print("[ERROR] JOB parameter is required.")
            print("[FUJITSU] SMBJOB JOB=jobname,PGM=program[,PARA=parameters][,@LIB=library][,VOL=volume]")
            set_pgmec(999)
            return False
        
        if not params['PGM']:
            print("[ERROR] PGM parameter is required.")
            print("[FUJITSU] SMBJOB JOB=jobname,PGM=program[,PARA=parameters][,@LIB=library][,VOL=volume]")
            set_pgmec(999)
            return False
        
        job_name = params['JOB']
        program = params['PGM']
        library = params['LIB']
        volume = params['VOL']
        parameters = params['PARA']
        jobq = params['JOBQ']
        jobk = params['JOBK']
        hold = params['HLDJOB'].upper() in ['@YES', 'YES', 'TRUE', '1']
        priority = int(params['JOBPTYY']) if params['JOBPTYY'].isdigit() else 5
        
        print(f"[INFO] Submitting job: {job_name}")
        print(f"[INFO] Program: {program}")
        print(f"[INFO] Library: {library if library else 'Auto-detect'}")
        print(f"[INFO] Volume: {volume}")
        print(f"[INFO] Parameters: {parameters if parameters else 'None'}")
        print(f"[INFO] Job Queue: {jobq}")
        print(f"[INFO] Priority: {priority}")
        print(f"[INFO] Hold Job: {'Yes' if hold else 'No'}")
        
        # Find program library if not specified
        if not library:
            library = _find_program_library(volume, program)
            if not library:
                print(f"[ERROR] Program '{program}' not found in any library on volume '{volume}'.")
                set_pgmec(999)
                return False
            print(f"[INFO] Program found in library: {library}")
        
        # Validate program exists in catalog
        catalog = get_catalog_info()
        if (volume not in catalog or library not in catalog[volume] or 
            program not in catalog[volume][library]):
            print(f"[ERROR] Program '{program}' not registered in catalog.json")
            set_pgmec(999)
            return False
        
        program_info = catalog[volume][library][program]
        
        if program_info.get('TYPE') != 'PGM':
            print(f"[ERROR] '{program}' is not a program (TYPE: {program_info.get('TYPE', 'Unknown')})")
            set_pgmec(999)
            return False
        
        # Generate unique job ID
        job_id = f"J{int(time.time())}{uuid.uuid4().hex[:6].upper()}"
        
        # Create job info
        job_info = JobInfo(
            job_id=job_id,
            job_name=job_name,
            program=program,
            library=library,
            volume=volume,
            parameters=parameters,
            priority=priority,
            jobq=jobq,
            jobk=jobk,
            hold=hold
        )
        
        # Add to job tracking
        ACTIVE_JOBS[job_id] = job_info
        
        if hold:
            job_info.status = "HELD"
            print(f"[INFO] Job {job_id} ({job_name}) submitted and held")
        else:
            # Submit to job queue for execution
            JOB_QUEUE.put((priority, job_id))
            print(f"[INFO] Job {job_id} ({job_name}) submitted to queue")
            
            # Start job processor if not running
            _ensure_job_processor_running()
        
        # Write job submission log
        _write_job_log(job_info, "JOB_SUBMITTED", f"Job {job_name} submitted successfully")
        
        print(f"[INFO] Job ID: {job_id}")
        print(f"[INFO] Log file: {job_info.log_file}")
        
        return True
        
    except Exception as e:
        print(f"[ERROR] SMBJOB command failed: {e}")
        set_pgmec(999)
        return False

def _find_program_library(volume: str, program: str) -> Optional[str]:
    """Find which library contains the specified program"""
    try:
        catalog = get_catalog_info()
        if volume in catalog:
            for lib_name, lib_content in catalog[volume].items():
                if program in lib_content and lib_content[program].get('TYPE') == 'PGM':
                    return lib_name
    except Exception as e:
        print(f"[DEBUG] Library search error: {e}")
    return None

def _ensure_job_processor_running():
    """Ensure job processor thread is running"""
    global _job_processor_thread
    if not hasattr(_ensure_job_processor_running, '_job_processor_thread') or \
       not _ensure_job_processor_running._job_processor_thread.is_alive():
        _ensure_job_processor_running._job_processor_thread = threading.Thread(
            target=_job_processor_worker, daemon=True)
        _ensure_job_processor_running._job_processor_thread.start()
        print("[INFO] Job processor thread started")

def _job_processor_worker():
    """Background job processor worker thread"""
    print("[INFO] Job processor worker started")
    
    while True:
        try:
            # Wait for job with timeout
            try:
                priority, job_id = JOB_QUEUE.get(timeout=5)
            except queue.Empty:
                continue
            
            if job_id not in ACTIVE_JOBS:
                print(f"[WARNING] Job {job_id} not found in active jobs")
                continue
            
            job_info = ACTIVE_JOBS[job_id]
            
            # Skip held jobs
            if job_info.status == "HELD":
                print(f"[INFO] Job {job_id} is held, skipping")
                continue
            
            print(f"[INFO] Processing job {job_id} ({job_info.job_name})")
            _execute_job(job_info)
            
        except Exception as e:
            print(f"[ERROR] Job processor error: {e}")
            time.sleep(1)

def _execute_job(job_info: JobInfo):
    """Execute a submitted job"""
    try:
        job_info.status = "RUNNING"
        job_info.start_time = datetime.now()
        
        _write_job_log(job_info, "JOB_STARTED", f"Job {job_info.job_name} started execution")
        
        print(f"[INFO] Executing job {job_info.job_id}: {job_info.job_name}")
        
        # Get program information
        catalog = get_catalog_info()
        program_info = catalog[job_info.volume][job_info.library][job_info.program]
        pgm_type = program_info.get('PGMTYPE', 'UNKNOWN').upper()
        
        # Execute based on program type
        success = False
        if pgm_type == 'JAVA':
            success = _execute_java_job(job_info, program_info)
        elif pgm_type == 'COBOL':
            success = _execute_cobol_job(job_info, program_info)
        elif pgm_type == 'SHELL':
            success = _execute_shell_job(job_info, program_info)
        elif pgm_type == 'PYTHON':
            success = _execute_python_job(job_info, program_info)
        else:
            _write_job_log(job_info, "JOB_ERROR", f"Unsupported program type: {pgm_type}")
            success = False
        
        # Update job status
        job_info.end_time = datetime.now()
        if success:
            job_info.status = "COMPLETED"
            _write_job_log(job_info, "JOB_COMPLETED", f"Job {job_info.job_name} completed successfully")
        else:
            job_info.status = "ERROR"
            _write_job_log(job_info, "JOB_ERROR", f"Job {job_info.job_name} failed")
        
        duration = (job_info.end_time - job_info.start_time).total_seconds()
        print(f"[INFO] Job {job_info.job_id} finished in {duration:.2f} seconds")
        
    except Exception as e:
        job_info.status = "ERROR"
        job_info.end_time = datetime.now()
        _write_job_log(job_info, "JOB_ERROR", f"Job execution error: {e}")
        print(f"[ERROR] Job {job_info.job_id} execution failed: {e}")

def _execute_java_job(job_info: JobInfo, program_info: Dict[str, Any]) -> bool:
    """Execute Java program in job context"""
    try:
        program_path = os.path.join(VOLUME_ROOT, job_info.volume, job_info.library)
        
        # Determine Java execution method
        jar_file = program_info.get('JARFILE')
        class_file = program_info.get('CLASSFILE')
        pgm_name = program_info.get('PGMNAME', job_info.program)
        
        if jar_file:
            jar_path = os.path.join(program_path, jar_file)
            if not os.path.exists(jar_path):
                _write_job_log(job_info, "JOB_ERROR", f"JAR file not found: {jar_path}")
                return False
            cmd = ['java', '-jar', jar_path]
        elif class_file:
            class_path = os.path.join(program_path, class_file)
            if not os.path.exists(class_path):
                _write_job_log(job_info, "JOB_ERROR", f"Class file not found: {class_path}")
                return False
            class_name = class_file.replace('/', '.').replace('.class', '')
            cmd = ['java', '-cp', program_path, class_name]
        else:
            cmd = ['java', '-cp', program_path, pgm_name]
        
        # Add parameters
        if job_info.parameters:
            param_list = job_info.parameters.split(',')
            cmd.extend([p.strip() for p in param_list if p.strip()])
        
        # Execute with job context
        return _run_job_command(job_info, cmd, program_path)
        
    except Exception as e:
        _write_job_log(job_info, "JOB_ERROR", f"Java job execution error: {e}")
        return False

def _execute_shell_job(job_info: JobInfo, program_info: Dict[str, Any]) -> bool:
    """Execute Shell script in job context"""
    try:
        program_path = os.path.join(VOLUME_ROOT, job_info.volume, job_info.library)
        shell_file = program_info.get('SHELLFILE', f"{job_info.program}.sh")
        shell_path = os.path.join(program_path, shell_file)
        
        if not os.path.exists(shell_path):
            _write_job_log(job_info, "JOB_ERROR", f"Shell file not found: {shell_path}")
            return False
        
        os.chmod(shell_path, 0o755)
        cmd = ['bash', shell_path]
        
        # Add parameters
        if job_info.parameters:
            param_list = job_info.parameters.split(',')
            cmd.extend([p.strip() for p in param_list if p.strip()])
        
        return _run_job_command(job_info, cmd, program_path)
        
    except Exception as e:
        _write_job_log(job_info, "JOB_ERROR", f"Shell job execution error: {e}")
        return False

def _execute_python_job(job_info: JobInfo, program_info: Dict[str, Any]) -> bool:
    """Execute Python script in job context"""
    try:
        program_path = os.path.join(VOLUME_ROOT, job_info.volume, job_info.library)
        python_file = program_info.get('PYTHONFILE', f"{job_info.program}.py")
        python_path = os.path.join(program_path, python_file)
        
        if not os.path.exists(python_path):
            _write_job_log(job_info, "JOB_ERROR", f"Python file not found: {python_path}")
            return False
        
        cmd = ['python3', python_path]
        
        # Add parameters
        if job_info.parameters:
            param_list = job_info.parameters.split(',')
            cmd.extend([p.strip() for p in param_list if p.strip()])
        
        return _run_job_command(job_info, cmd, program_path)
        
    except Exception as e:
        _write_job_log(job_info, "JOB_ERROR", f"Python job execution error: {e}")
        return False

def _execute_cobol_job(job_info: JobInfo, program_info: Dict[str, Any]) -> bool:
    """Execute COBOL program in job context (placeholder)"""
    _write_job_log(job_info, "JOB_INFO", "COBOL execution not yet implemented")
    return True

def _run_job_command(job_info: JobInfo, cmd: List[str], cwd: str) -> bool:
    """Run job command with proper logging and error handling"""
    try:
        _write_job_log(job_info, "JOB_EXEC", f"Executing: {' '.join(cmd)}")
        
        # Set environment variables
        env = os.environ.copy()
        env['ASP_JOB_ID'] = job_info.job_id
        env['ASP_JOB_NAME'] = job_info.job_name
        env['ASP_VOLUME'] = job_info.volume
        env['ASP_LIBRARY'] = job_info.library
        env['ASP_PROGRAM'] = job_info.program
        
        # Execute command
        result = subprocess.run(
            cmd, 
            capture_output=True, 
            text=True,
            cwd=cwd,
            env=env,
            timeout=300  # 5 minute timeout for jobs
        )
        
        job_info.return_code = result.returncode
        
        # Log output
        if result.stdout:
            _write_job_log(job_info, "JOB_OUTPUT", result.stdout)
        
        if result.stderr:
            _write_job_log(job_info, "JOB_ERROR", result.stderr)
        
        _write_job_log(job_info, "JOB_INFO", f"Return code: {result.returncode}")
        
        return result.returncode == 0
        
    except subprocess.TimeoutExpired:
        _write_job_log(job_info, "JOB_ERROR", "Job execution timed out")
        return False
    except Exception as e:
        _write_job_log(job_info, "JOB_ERROR", f"Command execution error: {e}")
        return False

def _write_job_log(job_info: JobInfo, level: str, message: str):
    """Write job log entry"""
    try:
        timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        log_entry = f"[{timestamp}] [{level}] {message}\n"
        
        with open(job_info.log_file, 'a', encoding='utf-8') as f:
            f.write(log_entry)
            
    except Exception as e:
        print(f"[ERROR] Failed to write job log: {e}")

def list_jobs() -> List[Dict[str, Any]]:
    """List all active jobs (for monitoring)"""
    jobs = []
    for job_id, job_info in ACTIVE_JOBS.items():
        jobs.append({
            'job_id': job_id,
            'job_name': job_info.job_name,
            'program': job_info.program,
            'library': job_info.library,
            'volume': job_info.volume,
            'status': job_info.status,
            'priority': job_info.priority,
            'submitted_time': job_info.submitted_time.isoformat(),
            'start_time': job_info.start_time.isoformat() if job_info.start_time else None,
            'end_time': job_info.end_time.isoformat() if job_info.end_time else None,
            'return_code': job_info.return_code,
            'log_file': job_info.log_file
        })
    return jobs

def release_job(job_id: str) -> bool:
    """Release a held job"""
    if job_id in ACTIVE_JOBS:
        job_info = ACTIVE_JOBS[job_id]
        if job_info.status == "HELD":
            job_info.status = "PENDING"
            JOB_QUEUE.put((job_info.priority, job_id))
            _ensure_job_processor_running()
            _write_job_log(job_info, "JOB_RELEASED", f"Job {job_info.job_name} released")
            return True
    return False

def cancel_job(job_id: str) -> bool:
    """Cancel a job"""
    if job_id in ACTIVE_JOBS:
        job_info = ACTIVE_JOBS[job_id]
        if job_info.status in ["PENDING", "HELD"]:
            job_info.status = "CANCELLED"
            job_info.end_time = datetime.now()
            _write_job_log(job_info, "JOB_CANCELLED", f"Job {job_info.job_name} cancelled")
            return True
    return False


# For backwards compatibility and testing
if __name__ == "__main__":
    import sys
    if len(sys.argv) > 1:
        SMBJOB(' '.join(sys.argv[1:]))