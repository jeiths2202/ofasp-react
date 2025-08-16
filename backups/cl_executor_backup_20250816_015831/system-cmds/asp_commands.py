import subprocess
import os
import json
import psutil
import socket
from datetime import datetime
import shutil
import sys
import termios
import tty
import select
import locale
import curses

# UTF-8 encoding settings
os.environ['PYTHONIOENCODING'] = 'utf-8'
try:
    locale.setlocale(locale.LC_ALL, 'C.UTF-8')
except:
    try:
        locale.setlocale(locale.LC_ALL, 'en_US.UTF-8')
    except:
        pass

VOLUME_ROOT = "/home/aspuser/app/volume"
CONFIG_ROOT = "/home/aspuser/app/config"
PROFILE_DIR = os.path.join(CONFIG_ROOT, "profiles")
SYSVAL_FILE = os.path.join(CONFIG_ROOT, "system_values.json")
# Import centralized catalog configuration - no hardcoding
import sys
sys.path.append('/home/aspuser/app/config')
from catalog_config import get_catalog_path
CATALOG_FILE = get_catalog_path()
JOB_LOG_DIR = os.path.join(VOLUME_ROOT, "JOBLOG")

# Initialize configuration directories
os.makedirs(CONFIG_ROOT, exist_ok=True)
os.makedirs(PROFILE_DIR, exist_ok=True)
os.makedirs(JOB_LOG_DIR, exist_ok=True)

# Global variable for program execution code (@PGMEC)
_PGMEC = 0

def set_pgmec(value):
    """Set @PGMEC variable (program execution code)"""
    global _PGMEC
    _PGMEC = value

def get_pgmec():
    """Get @PGMEC variable (program execution code)"""
    return _PGMEC

def reset_pgmec():
    """Reset @PGMEC to 0 (successful execution)"""
    global _PGMEC
    _PGMEC = 0

def get_catalog_info():
    """Get file information from centralized catalog.json"""
    if os.path.exists(CATALOG_FILE):
        try:
            with open(CATALOG_FILE, 'r', encoding='utf-8') as f:
                return json.load(f)
        except:
            pass
    return {}

def update_catalog_info(volume, library, object_name, object_type="DATASET", **kwargs):
    """Update object information in catalog.json with new hierarchical structure"""
    catalog = get_catalog_info()
    
    # Ensure volume exists
    if volume not in catalog:
        catalog[volume] = {}
    
    # Ensure library exists in volume
    if library not in catalog[volume]:
        catalog[volume][library] = {}
    
    # Ensure object exists in library
    if object_name not in catalog[volume][library]:
        catalog[volume][library][object_name] = {}
    
    # Set object type
    catalog[volume][library][object_name]["TYPE"] = object_type
    
    # Add timestamp information
    current_time = datetime.now().isoformat() + "Z"
    if "CREATED" not in catalog[volume][library][object_name]:
        catalog[volume][library][object_name]["CREATED"] = current_time
    catalog[volume][library][object_name]["UPDATED"] = current_time
    
    # Handle different object types with appropriate attributes
    if object_type == "DATASET":
        # Set dataset-specific defaults
        catalog[volume][library][object_name]["RECTYPE"] = kwargs.get("RECTYPE", "FB")
        catalog[volume][library][object_name]["RECLEN"] = kwargs.get("RECLEN", 80)
        catalog[volume][library][object_name]["ENCODING"] = kwargs.get("ENCODING", "utf-8")
    elif object_type == "PGM":
        # Set program-specific defaults
        catalog[volume][library][object_name]["PGMTYPE"] = kwargs.get("PGMTYPE", "COBOL")
        catalog[volume][library][object_name]["VERSION"] = kwargs.get("VERSION", "1.0")
    elif object_type == "MAP":
        # Set map-specific defaults
        catalog[volume][library][object_name]["MAPTYPE"] = kwargs.get("MAPTYPE", "SMED")
        catalog[volume][library][object_name]["ROWS"] = kwargs.get("ROWS", 24)
        catalog[volume][library][object_name]["COLS"] = kwargs.get("COLS", 80)
    elif object_type == "JOB":
        # Set job-specific defaults
        catalog[volume][library][object_name]["JOBTYPE"] = kwargs.get("JOBTYPE", "BATCH")
        catalog[volume][library][object_name]["SCHEDULE"] = kwargs.get("SCHEDULE", "MANUAL")
    
    # Update additional attributes
    for key, value in kwargs.items():
        catalog[volume][library][object_name][key] = value
    
    try:
        with open(CATALOG_FILE, 'w', encoding='utf-8') as f:
            json.dump(catalog, f, indent=2, ensure_ascii=False)
    except Exception as e:
        print(f"[WARNING] catalog.json update failed: {e}")

def get_object_info(volume, library, object_name):
    """Get catalog information for a specific object"""
    catalog = get_catalog_info()
    return catalog.get(volume, {}).get(library, {}).get(object_name, {})

def get_file_info(volume, filename):
    """Legacy function: Get catalog information for a specific file (backward compatibility)"""
    # Parse library/filename if in format LIB/FILE
    if '/' in filename:
        library, object_name = filename.split('/', 1)
        return get_object_info(volume, library, object_name)
    else:
        # Search through all libraries for the file
        catalog = get_catalog_info()
        volume_data = catalog.get(volume, {})
        for library, objects in volume_data.items():
            if filename in objects:  # Fixed: use 'filename' instead of undefined 'object_name'
                return objects[filename]
        # Return default for dataset if not found
        return {
            "TYPE": "DATASET",
            "RECTYPE": "FB",
            "RECLEN": 80,
            "ENCODING": "utf-8"
        }

def _convert_bytes_to_string(data, encoding='utf-8'):
    """Convert bytes to string using Java API or fallback methods"""
    if not data:
        return ""
    
    try:
        # First try Java API
        from java_encoding_client import convert_bytes_to_string_via_java
        result = convert_bytes_to_string_via_java(data, encoding)
        
        # Check if Java API returned a valid UTF-8 string (not hex fallback)
        if result and not all(c in '0123456789ABCDEF .' for c in result[:20]):
            return result
            
    except Exception as e:
        print(f"[WARNING] Java API encoding failed: {e}")
    
    try:
        # Fallback to external tools
        encoding_map = {
            'shift_jis': 'SHIFT_JIS',
            'shift-jis': 'SHIFT_JIS', 
            'sjis': 'SHIFT_JIS',
            'utf-8': 'UTF-8',
            'euc-jp': 'EUC-JP',
            'iso-2022-jp': 'ISO-2022-JP'
        }
        
        # Try with nkf for any Japanese encoding
        if encoding.lower() in ['shift_jis', 'shift-jis', 'shift.jis', 'sjis']:
            result = subprocess.run(
                ['nkf', '-w', '-S'], 
                input=data, 
                capture_output=True, 
                timeout=5
            )
            if result.returncode == 0:
                return result.stdout.decode('utf-8', errors='replace')
        
        # Try with iconv for all encodings
        iconv_encoding = encoding_map.get(encoding.lower(), encoding.upper())
        result = subprocess.run(
            ['iconv', '-f', iconv_encoding, '-t', 'UTF-8'],
            input=data,
            capture_output=True,
            timeout=5
        )
        if result.returncode == 0:
            return result.stdout.decode('utf-8', errors='replace')
            
    except (subprocess.TimeoutExpired, subprocess.CalledProcessError, FileNotFoundError, OSError):
        pass
    
    # Absolute fallback: show hex representation of entire data
    return ' '.join(f'{b:02X}' for b in data)



def WRKOBJ(command):
    # Example: WRKOBJ TYPE-DATASET,VOL-DISK01,LIB-TESTLIB
    # Example: WRKOBJ TYPE-PGM,VOL-DISK01
    # Example: WRKOBJ TYPE-ALL,VOL-DISK01,LIB-TESTLIB
    params = dict(item.split('-') for item in command.replace('WRKOBJ ', '').split(','))
    obj_type = params.get('TYPE', 'ALL')
    vol = params.get('VOL')
    lib = params.get('LIB')

    if not vol:
        print("[ERROR] VOL parameter is required.")
        return

    catalog = get_catalog_info()
    volume_data = catalog.get(vol, {})
    
    if not volume_data:
        print(f"[ERROR] Volume '{vol}' not found in catalog.")
        return

    # Filter by library if specified
    libraries_to_check = [lib] if lib else list(volume_data.keys())
    
    print(f"[INFO] Objects in volume '{vol}':")
    print("=" * 80)
    
    total_objects = 0
    for library_name in libraries_to_check:
        if library_name not in volume_data:
            if lib:  # Only show error if specific library was requested
                print(f"[ERROR] Library '{library_name}' not found in volume '{vol}'.")
            continue
            
        library_objects = volume_data[library_name]
        if not library_objects:
            continue
            
        print(f"\nLibrary: {library_name}")
        print("-" * 40)
        
        # Group objects by type
        objects_by_type = {}
        for obj_name, obj_info in library_objects.items():
            obj_obj_type = obj_info.get('TYPE', 'UNKNOWN')
            if obj_obj_type not in objects_by_type:
                objects_by_type[obj_obj_type] = []
            objects_by_type[obj_obj_type].append((obj_name, obj_info))
        
        # Display objects filtered by type
        for current_type in sorted(objects_by_type.keys()):
            if obj_type != 'ALL' and obj_type != current_type:
                continue
                
            print(f"\n  {current_type} Objects:")
            for obj_name, obj_info in sorted(objects_by_type[current_type]):
                total_objects += 1
                
                # Get filesystem info if file exists
                file_path = os.path.join(VOLUME_ROOT, vol, library_name, obj_name)
                size_info = "N/A"
                mtime_info = "N/A"
                
                if os.path.exists(file_path):
                    size = os.path.getsize(file_path)
                    mtime = datetime.fromtimestamp(os.path.getmtime(file_path)).strftime('%Y-%m-%d %H:%M:%S')
                    size_info = f"{size} bytes"
                    mtime_info = mtime
                
                # Display object details based on type
                print(f"    {obj_name.ljust(15)} |", end="")
                
                if current_type == "DATASET":
                    rectype = obj_info.get('RECTYPE', 'FB')
                    reclen = obj_info.get('RECLEN', 80)
                    encoding = obj_info.get('ENCODING', 'utf-8')
                    print(f" {rectype}-{reclen} | {encoding} | {size_info} | {mtime_info}")
                elif current_type == "PGM":
                    pgmtype = obj_info.get('PGMTYPE', 'UNKNOWN')
                    version = obj_info.get('VERSION', '1.0')
                    print(f" {pgmtype} | V{version} | {size_info} | {mtime_info}")
                elif current_type == "MAP":
                    maptype = obj_info.get('MAPTYPE', 'SMED')
                    rows = obj_info.get('ROWS', 24)
                    cols = obj_info.get('COLS', 80)
                    print(f" {maptype} | {rows}x{cols} | {size_info} | {mtime_info}")
                elif current_type == "JOB":
                    jobtype = obj_info.get('JOBTYPE', 'BATCH')
                    schedule = obj_info.get('SCHEDULE', 'MANUAL')
                    print(f" {jobtype} | {schedule} | {size_info} | {mtime_info}")
                else:
                    print(f" {size_info} | {mtime_info}")
                
                # Show description if available
                description = obj_info.get('DESCRIPTION')
                if description:
                    print(f"                     Description: {description}")

    print("=" * 80)
    print(f"Total objects found: {total_objects}")
    
    if obj_type != 'ALL':
        print(f"Filter applied: TYPE={obj_type}")
    if lib:
        print(f"Library filter: {lib}")

def RCVMSG(command):
    params = dict(item.split('-', 1) for item in command.replace('RCVMSG ', '').split(','))
    user = params.get('USER')

    if not user:
        print("[ERROR] USER parameter is missing.")
        return

    user_file = os.path.join(VOLUME_ROOT, "MSGQ", "users", f"{user}.msg")
    if not os.path.isfile(user_file):
        print(f"[INFO] User '{user}'has no received messages.")
        return

    print(f"[INFO] User '{user}'received messages:")
    with open(user_file, 'r') as f:
        for line in f:
            print(" ", line.strip())
def SNDMSG(command):
    params = dict(item.split('-', 1) for item in command.replace('SNDMSG ', '').split(','))
    user = params.get('TO')
    message = params.get('MSG')

    if not user or not message:
        print("[ERROR] TO or MSG parameter is missing.")
        return

    user_dir = os.path.join(VOLUME_ROOT, "MSGQ", "users")
    os.makedirs(user_dir, exist_ok=True)
    user_file = os.path.join(user_dir, f"{user}.msg")

    timestamp = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    with open(user_file, 'a') as f:
        f.write(f"[{timestamp}] {message}\n")

    print(f"[INFO] User '{user}'message sent.")
    log_message("INFO", f"SNDMSG TO-{user}: {message}")
def RSTLIB(command):
    params = dict(item.split('-') for item in command.replace('RSTLIB ', '').split(','))
    backup_file = params.get('FILE')

    if not backup_file:
        print("[ERROR] FILE parameter is missing.")
        return

    backup_path = os.path.join(BACKUP_DIR, backup_file)
    if not os.path.isfile(backup_path):
        print(f"[ERROR] Backup file does not exist: {backup_path}")
        return

    try:
        with tarfile.open(backup_path, "r:gz") as tar:
            tar.extractall(path=VOLUME_ROOT)
        print(f"[INFO] Restore completed: {backup_path}")
        log_message("INFO", f"RSTLIB → {backup_file} restore successful")
    except Exception as e:
        print(f"[ERROR] Restore failed: {e}")
        log_message("ERROR", f"RSTLIB failed: {e}")
import tarfile

def SAVLIB(command):
    params = dict(item.split('-') for item in command.replace('SAVLIB ', '').split(','))
    lib = params.get('LIB')
    vol = params.get('VOL')

    if not lib or not vol:
        print("[ERROR] LIB or VOL parameter is missing.")
        return

    lib_path = os.path.join(VOLUME_ROOT, vol, lib)
    if not os.path.isdir(lib_path):
        print(f"[ERROR] Library '{lib}'does not exist in volume '{vol}'.")
        return

    timestamp = datetime.now().strftime("%Y%m%d%H%M%S")
    backup_name = f"{lib}_{vol}_{timestamp}.tar.gz"
    backup_path = os.path.join(BACKUP_DIR, backup_name)

    with tarfile.open(backup_path, "w:gz") as tar:
        tar.add(lib_path, arcname=f"{lib}")

    print(f"[INFO] Library '{lib}'has been backed up: {backup_path}")
    log_message("INFO", f"SAVLIB {lib} → {backup_name}")
def DSPJOB(command=None):
    """Enhanced DSPJOB - Display Job Information and System Variables"""
    print("[INFO] DSPJOB - Display Job Information and System Variables")
    
    # Display system variables
    print("\n=== System Variables ===")
    print(f"  @PGMEC (Program Exit Code): {get_pgmec()}")
    print(f"  @USER (Current User): admin")  # TODO: Get from session
    print(f"  @DATE (Current Date): {datetime.now().strftime('%Y-%m-%d')}")
    print(f"  @TIME (Current Time): {datetime.now().strftime('%H:%M:%S')}")
    print(f"  @JOB (Current Job): ASP_TERMINAL_{datetime.now().strftime('%Y%m%d')}")
    
    # Display process information
    try:
        import psutil
        current_process = psutil.Process()
        print(f"\n=== Process Information ===")
        print(f"  Process ID: {current_process.pid}")
        print(f"  Memory Usage: {current_process.memory_info().rss // 1024 // 1024} MB")
        print(f"  CPU Time: {current_process.cpu_times().user + current_process.cpu_times().system:.2f} seconds")
    except ImportError:
        print(f"\n=== Process Information ===")
        print("  psutil not available - basic info only")
        print(f"  Process ID: {os.getpid()}")
    
    # Display environment information
    print(f"\n=== Environment ===")
    print(f"  Volume Root: {VOLUME_ROOT}")
    print(f"  Config Root: {CONFIG_ROOT}")
    print(f"  Job Log Dir: {JOB_LOG_DIR}")
    print(f"  Python Version: {sys.version.split()[0]}")
    
    # Display job history
    log_path = os.path.join(VOLUME_ROOT, "JOBLOG", "job.log")
    if os.path.isfile(log_path):
        print(f"\n=== Recent Job History ===")
        try:
            with open(log_path, "r") as f:
                lines = f.readlines()
                if lines:
                    for line in reversed(lines[-5:]):  # Show last 5 jobs
                        try:
                            job_id, lib, prog, start, end, status = line.strip().split(',')
                            print(f"  [JOB] {job_id}")
                            print(f"    |- Program: {lib}/{prog}")
                            print(f"    |- Start: {start}")
                            print(f"    |- End: {end}")
                            print(f"    |- Status: {status}")
                        except ValueError:
                            continue
                else:
                    print("  No job history available.")
        except Exception as e:
            print(f"  Error reading job history: {e}")
    else:
        print(f"\n=== Recent Job History ===")
        print("  No job log file found.")
def record_job(lib, prog, status, start_time, end_time):
    job_dir = os.path.join(VOLUME_ROOT, "JOBLOG")
    os.makedirs(job_dir, exist_ok=True)
    log_path = os.path.join(job_dir, "job.log")
    job_id = datetime.now().strftime("%Y%m%d%H%M%S")
    with open(log_path, "a") as f:
        f.write(f"{job_id},{lib},{prog},{start_time},{end_time},{status}\n")
def WRKMSG(command=None):
    log_path = os.path.join(VOLUME_ROOT, "MSGQ", "system.log")
    if not os.path.isfile(log_path):
        print("[INFO] No messages in message queue.")
        return

    print("[INFO] System message queue:")
    with open(log_path, "r") as f:
        for line in f:
            print(" ", line.strip())
def log_message(level, message):
    msgq_dir = os.path.join(VOLUME_ROOT, "MSGQ")
    os.makedirs(msgq_dir, exist_ok=True)
    log_path = os.path.join(msgq_dir, "system.log")
    now = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    with open(log_path, "a") as f:
        f.write(f"[{level.upper()}] {now} {message}\n")
def WRKVOL(command=None):
    print("[INFO] Volume Status:")

    if not os.path.isdir(VOLUME_ROOT):
        print("[INFO] No volumes are currently registered.")
        return

    for vol in os.listdir(VOLUME_ROOT):
        vol_path = os.path.join(VOLUME_ROOT, vol)
        if not os.path.isdir(vol_path):
            continue

        lib_count = 0
        file_count = 0
        total_size = 0

        for lib in os.listdir(vol_path):
            lib_path = os.path.join(vol_path, lib)
            if not os.path.isdir(lib_path):
                continue

            lib_count += 1
            for obj in os.listdir(lib_path):
                obj_path = os.path.join(lib_path, obj)
                if os.path.isfile(obj_path):
                    file_count += 1
                    total_size += os.path.getsize(obj_path)

        print(f"  Volume Name        : {vol}")
        print(f"     |- Library Count : {lib_count}")
        print(f"     |- Total Files   : {file_count}")
        print(f"     -- Disk Usage    : {total_size:,} Byte")

def CALL(command):
    """
    CALL command - delegated to functions.call module
    
    Enhanced implementation with multi-language support, SMED integration,
    and comprehensive parameter parsing.
    """
    from functions.call import CALL as call_impl
    return call_impl(command)

def DSPFD(command):
    """
    DSPFD command - delegated to functions.dspfd module
    
    Enhanced implementation with catalog integration and detailed
    file information display.
    """
    from functions.dspfd import DSPFD as dspfd_impl
    return dspfd_impl(command)

def DLTFILE(command):
    """
    DLTFILE command - delegated to functions.dltfile module
    
    Enhanced implementation with better validation, error handling,
    and comprehensive catalog cleanup.
    """
    from functions.dltfile import DLTFILE as dltfile_impl
    return dltfile_impl(command)
def DLTLIB(command):
    """
    DLTLIB command - delegated to functions.dltlib module
    
    Enhanced implementation with better validation, error handling,
    and library usage statistics.
    """
    from functions.dltlib import DLTLIB as dltlib_impl
    return dltlib_impl(command)

def CRTLIB(command):
    """
    CRTLIB command - delegated to functions.crtlib module
    
    Enhanced implementation with better validation and error handling
    for library creation.
    """
    from functions.crtlib import CRTLIB as crtlib_impl
    return crtlib_impl(command)

def CRTPGM(command):
    # Example: CRTPGM PGM(TESTLIB/PAYROLL01),VOL-DISK01,PGMTYPE-COBOL,VERSION-2.1,DESC-'Monthly payroll calculation'
    main_part, *others = command.replace('CRTPGM ', '').split(',')
    pgm_lib, pgm_name = main_part.replace('PGM(', '').replace(')', '').split('/')
    params = dict(item.split('-', 1) for item in others if '-' in item)
    vol = params.get('VOL')

    if not vol:
        print("[ERROR] VOL parameter is missing.")
        return

    lib_path = os.path.join(VOLUME_ROOT, vol, pgm_lib)
    
    if not os.path.exists(lib_path):
        print(f"[ERROR] Library '{pgm_lib}' does not exist. Please run CRTLIB command first.")
        return

    # Parse program attributes
    pgmtype = params.get('PGMTYPE', 'COBOL')
    version = params.get('VERSION', '1.0')
    description = params.get('DESC', f'{pgm_name} program').strip("'\"")
    pgmname = params.get('PGMNAME', pgm_name)
    
    # Type-specific attributes
    program_attrs = {
        'PGMNAME': pgmname,
        'VERSION': version,
        'DESCRIPTION': description
    }
    
    if pgmtype.upper() == 'JAVA':
        program_attrs['JARFILE'] = params.get('JARFILE', f'{pgm_name}.jar')
    elif pgmtype.upper() == 'COBOL':
        program_attrs['SOURCEFILE'] = params.get('SOURCEFILE', f'{pgm_name}.cbl')
        program_attrs['EXECUTABLE'] = params.get('EXECUTABLE', pgm_name)
    elif pgmtype.upper() == 'SHELL':
        program_attrs['SHELLFILE'] = params.get('SHELLFILE', f'{pgm_name}.sh')

    # Create program entry in filesystem (placeholder)
    pgm_path = os.path.join(lib_path, pgm_name)
    with open(pgm_path, 'w', encoding='utf-8') as f:
        f.write(f"# {pgmtype} Program: {pgm_name}\n")
        f.write(f"# Description: {description}\n")
        f.write(f"# Version: {version}\n")

    # Update catalog with new hierarchical structure
    update_catalog_info(
        volume=vol, 
        library=pgm_lib, 
        object_name=pgm_name,
        object_type="PGM",
        PGMTYPE=pgmtype,
        **program_attrs
    )
    
    print(f"[INFO] Program '{pgm_name}' in library '{pgm_lib}' has been created: {pgm_path}")
    print(f"[INFO] Program registered in catalog.json:")
    print(f"       TYPE: PGM")
    print(f"       PGMTYPE: {pgmtype}")
    print(f"       VERSION: {version}")
    if pgmtype.upper() == 'JAVA':
        print(f"       JARFILE: {program_attrs.get('JARFILE')}")
    elif pgmtype.upper() == 'COBOL':
        print(f"       SOURCEFILE: {program_attrs.get('SOURCEFILE')}")

def CRTMAP(command):
    # Example: CRTMAP MAP(TESTLIB/MAINMENU),VOL-DISK01,MAPTYPE-SMED,ROWS-24,COLS-80,DESC-'Main menu screen'
    main_part, *others = command.replace('CRTMAP ', '').split(',')
    map_lib, map_name = main_part.replace('MAP(', '').replace(')', '').split('/')
    params = dict(item.split('-', 1) for item in others if '-' in item)
    vol = params.get('VOL')

    if not vol:
        print("[ERROR] VOL parameter is missing.")
        return

    lib_path = os.path.join(VOLUME_ROOT, vol, map_lib)
    
    if not os.path.exists(lib_path):
        print(f"[ERROR] Library '{map_lib}' does not exist. Please run CRTLIB command first.")
        return

    # Parse map attributes
    maptype = params.get('MAPTYPE', 'SMED')
    rows = int(params.get('ROWS', '24'))
    cols = int(params.get('COLS', '80'))
    description = params.get('DESC', f'{map_name} screen map').strip("'\"")
    
    # Type-specific attributes
    map_attrs = {
        'ROWS': rows,
        'COLS': cols,
        'DESCRIPTION': description
    }
    
    if maptype.upper() == 'SMED':
        map_attrs['MAPFILE'] = params.get('MAPFILE', f'{map_name}.smed')
    elif maptype.upper() == 'HTML':
        map_attrs['MAPFILE'] = params.get('MAPFILE', f'{map_name}.html')
        map_attrs['RESPONSIVE'] = params.get('RESPONSIVE', 'true').lower() == 'true'

    # Create map file in filesystem
    map_path = os.path.join(lib_path, map_name)
    with open(map_path, 'w', encoding='utf-8') as f:
        f.write(f"# {maptype} Map: {map_name}\n")
        f.write(f"# Description: {description}\n")
        f.write(f"# Dimensions: {rows}x{cols}\n")

    # Update catalog with new hierarchical structure
    update_catalog_info(
        volume=vol, 
        library=map_lib, 
        object_name=map_name,
        object_type="MAP",
        MAPTYPE=maptype,
        **map_attrs
    )
    
    print(f"[INFO] Map '{map_name}' in library '{map_lib}' has been created: {map_path}")
    print(f"[INFO] Map registered in catalog.json:")
    print(f"       TYPE: MAP")
    print(f"       MAPTYPE: {maptype}")
    print(f"       DIMENSIONS: {rows}x{cols}")
    if maptype.upper() == 'HTML':
        print(f"       RESPONSIVE: {map_attrs.get('RESPONSIVE', False)}")

def CRTFILE(command):
    """
    CRTFILE command - delegated to functions.crtfile module
    
    Enhanced implementation with better validation and error handling
    for dataset creation and catalog management.
    """
    from functions.crtfile import CRTFILE as crtfile_impl
    return crtfile_impl(command)
    
def WRKLIB(command=None):
    print(f"[INFO] All libraries in '{VOLUME_ROOT}':")
    if not os.path.exists(VOLUME_ROOT):
        print("[INFO] No volumes have been created yet.")
        return

    for vol in os.listdir(VOLUME_ROOT):
        vol_path = os.path.join(VOLUME_ROOT, vol)
        if os.path.isdir(vol_path):
            print(f" Volume: {vol}")
            for lib in os.listdir(vol_path):
                if os.path.isdir(os.path.join(vol_path, lib)):
                    print(f"   - Library: {lib}")
                
def WRKSPLF(command=None):
    splf_root = os.path.join(VOLUME_ROOT, "SPLF")

    if not os.path.isdir(splf_root):
        print("[INFO] No spool files currently exist.")
        return

    print("[INFO] Spool file list:")
    for lib in os.listdir(splf_root):
        lib_path = os.path.join(splf_root, lib)
        if not os.path.isdir(lib_path):
            continue

        for logfile in os.listdir(lib_path):
            log_path = os.path.join(lib_path, logfile)
            size = os.path.getsize(log_path)
            mtime = datetime.fromtimestamp(os.path.getmtime(log_path)).strftime('%Y-%m-%d %H:%M:%S')
            print(f"  [FILE] {lib}/{logfile.ljust(20)} | Size: {size:>6} Byte | Modified: {mtime}")

def EDTFILE(command):
    """
    EDTFILE command - delegated to functions.edtfile module
    
    Enhanced implementation with full interactive editing capabilities
    based on Fujitsu ASP EDTFILE manual specifications.
    """
    from functions.edtfile import EDTFILE as edtfile_impl
    return edtfile_impl(command)

# EDTFILE helper functions moved to functions.edtfile module


def CTTFILE(command):
    """Display records with pagination control"""
    page_size = 20
    current_page = 0
    total_pages = (len(records) + page_size - 1) // page_size
    
    # Save terminal settings
    old_settings = None
    try:
        old_settings = termios.tcgetattr(sys.stdin)
        tty.setraw(sys.stdin.fileno())
    except:
        pass
    
    try:
        while True:
            # Clear screen and show header
            os.system('clear')
            print(f"EDTFILE - {file_path}")
            print(f"Page {current_page + 1}/{total_pages} (Total {len(records)}records)")
            print("=" * 80)
            
            # Show current page records
            start_idx = current_page * page_size
            end_idx = min(start_idx + page_size, len(records))
            
            for i in range(start_idx, end_idx):
                record_num, record_data = records[i]
                # Truncate long lines
                display_data = record_data[:75] if len(record_data) > 75 else record_data
                print(f"{record_num:>6}: {display_data}")
            
            # Show remaining empty lines
            for i in range(end_idx - start_idx, page_size):
                print()
            
            print("=" * 80)
            print("Commands: [N]ext, [P]rev, [F]irst, [L]ast, [Q]uit, [G]oto line")
            print(f"Current position: {start_idx + 1}-{end_idx} / {len(records)}")
            
            # Get user input
            if old_settings:
                key = sys.stdin.read(1).upper()
            else:
                key = input("Enter command: ").upper()
            
            if key == 'Q':
                break
            elif key == 'N':
                if current_page < total_pages - 1:
                    current_page += 1
            elif key == 'P':
                if current_page > 0:
                    current_page -= 1
            elif key == 'F':
                current_page = 0
            elif key == 'L':
                current_page = total_pages - 1
            elif key == 'G':
                if old_settings:
                    # Restore terminal for input
                    termios.tcsetattr(sys.stdin, termios.TCSADRAIN, old_settings)
                try:
                    line_num = int(input("\nEnter line number to go to: "))
                    if 1 <= line_num <= len(records):
                        current_page = (line_num - 1) // page_size
                    else:
                        print(f"Line number must be between 1-{len(records)} range.")
                        input("Press Enter to continue...")
                except ValueError:
                    print("Please enter a valid number.")
                    input("Press Enter to continue...")
                finally:
                    if old_settings:
                        tty.setraw(sys.stdin.fileno())
            
    finally:
        # Restore terminal settings
        if old_settings:
            termios.tcsetattr(sys.stdin, termios.TCSADRAIN, old_settings)
        
        os.system('clear')
        print(f"[INFO] EDTFILE terminated")

def _launch_curses_browser(file_path, records, raw_records, rectype, reclen, encoding):
    """Launch curses-based record browser with hex display and advanced features"""
    def browser_main(stdscr):
        # Initialize curses
        if curses.has_colors():
            curses.start_color()
            curses.use_default_colors()
            curses.init_pair(1, curses.COLOR_WHITE, curses.COLOR_BLUE)   # Header
            curses.init_pair(2, curses.COLOR_BLACK, curses.COLOR_YELLOW) # Cursor position
            curses.init_pair(3, curses.COLOR_GREEN, -1)  # Hex display
            curses.init_pair(4, curses.COLOR_RED, -1)    # Status
            curses.init_pair(5, curses.COLOR_CYAN, -1)   # Normal text
            curses.init_pair(6, curses.COLOR_WHITE, curses.COLOR_BLACK) # Help window
            curses.init_pair(7, curses.COLOR_MAGENTA, -1) # Hex overlay
        
        # Configure terminal
        stdscr.keypad(True)
        curses.noecho()
        curses.cbreak()
        curses.curs_set(1)  # Show cursor
        
        max_y, max_x = stdscr.getmaxyx()
        
        # State variables
        current_record = 0
        current_col = 0
        top_record = 0
        hex_overlay = False
        command_mode = False
        command_buffer = ""
        
        def draw_help_window():
            """Draw F1 help window"""
            help_lines = [
                "EDTFILE Browser Help",
                "=" * 30,
                "Navigation:",
                "  ↑/↓     - Move between records",
                "  ←/→     - Move cursor in record",
                "  PgUp/PgDn - Page up/down",
                "  Home/End - First/last record",
                "",
                "Commands:",
                "  :hexon  - Show hex values below data",
                "  :hexoff - Hide hex values",
                "  ESC     - Exit command mode",
                "  q       - Quit browser",
                "",
                "Press any key to close help"
            ]
            
            # Calculate window position
            help_height = len(help_lines) + 2
            help_width = 40
            help_y = (max_y - help_height) // 2
            help_x = (max_x - help_width) // 2
            
            # Draw help window
            for i in range(help_height):
                for j in range(help_width):
                    try:
                        stdscr.addch(help_y + i, help_x + j, ' ', curses.color_pair(6))
                    except:
                        pass
            
            # Draw help content
            for i, line in enumerate(help_lines):
                try:
                    stdscr.addstr(help_y + 1 + i, help_x + 2, line[:help_width-4], 
                                 curses.color_pair(6) | curses.A_BOLD)
                except:
                    pass
            
            stdscr.refresh()
            stdscr.getch()
        
        # Main display loop
        while True:
            try:
                stdscr.clear()
                
                # Calculate display areas
                if hex_overlay:
                    visible_records = (max_y - 2) // 2  # Each record takes 2 lines with hex
                else:
                    visible_records = max_y - 2  # Reserve bottom lines for status
                
                # Adjust scrolling
                if current_record < top_record:
                    top_record = current_record
                elif current_record >= top_record + visible_records:
                    top_record = current_record - visible_records + 1
                
                # Display records (starting from top)
                display_y = 0
                for i in range(visible_records):
                    record_idx = top_record + i
                    if record_idx >= len(records):
                        break
                    
                    record_num, record_str = records[record_idx]
                    raw_data = raw_records[record_idx] if record_idx < len(raw_records) else b''
                    
                    # Show data line
                    if record_idx == current_record:
                        # Highlight current record and show cursor
                        for j, ch in enumerate(record_str):
                            if j == current_col:
                                stdscr.addstr(display_y, j, ch, curses.color_pair(2))
                            else:
                                stdscr.addstr(display_y, j, ch)
                    else:
                        stdscr.addstr(display_y, 0, record_str[:max_x])
                    
                    display_y += 1
                    
                    # Show hex overlay if enabled
                    if hex_overlay and display_y < max_y - 2:
                        hex_line = ""
                        for j in range(min(len(raw_data), max_x)):
                            hex_line += f"{raw_data[j]:02X} "
                        stdscr.addstr(display_y, 0, hex_line[:max_x], curses.color_pair(7))
                        display_y += 1
                
                # Show hex value of cursor position in status line
                status_y = max_y - 2
                if current_record < len(raw_records) and current_col < len(raw_records[current_record]):
                    byte_val = raw_records[current_record][current_col]
                    hex_info = f"Pos: {current_col:04X} | Hex: {byte_val:02X} | Dec: {byte_val:3d} | Char: '{chr(byte_val) if 32 <= byte_val <= 126 else '.'}'"
                    stdscr.addstr(status_y, 0, hex_info, curses.color_pair(3) | curses.A_BOLD)
                
                # Show command mode or help hint
                if command_mode:
                    stdscr.addstr(max_y - 1, 0, f":{command_buffer}", curses.color_pair(4))
                else:
                    stdscr.addstr(max_y - 1, 0, "F1=Help | q=Quit | :=Command", curses.color_pair(4))
                
                stdscr.refresh()
                
                # Handle input
                key = stdscr.getch()
                
                if command_mode:
                    if key == 27:  # ESC
                        command_mode = False
                        command_buffer = ""
                    elif key == ord('\n') or key == ord('\r'):
                        # Execute command
                        if command_buffer == "hexon":
                            hex_overlay = True
                        elif command_buffer == "hexoff":
                            hex_overlay = False
                        command_mode = False
                        command_buffer = ""
                    elif key == curses.KEY_BACKSPACE or key == 127:
                        command_buffer = command_buffer[:-1]
                    elif 32 <= key <= 126:
                        command_buffer += chr(key)
                else:
                    # Normal mode
                    if key == ord('q') or key == ord('Q'):
                        break
                    elif key == curses.KEY_F1:
                        draw_help_window()
                    elif key == ord(':'):
                        command_mode = True
                        command_buffer = ""
                    elif key == curses.KEY_UP:
                        if current_record > 0:
                            current_record -= 1
                            current_col = 0  # Reset to start of line
                    elif key == curses.KEY_DOWN:
                        if current_record < len(records) - 1:
                            current_record += 1
                            current_col = 0  # Reset to start of line
                    elif key == curses.KEY_LEFT:
                        if current_col > 0:
                            current_col -= 1
                    elif key == curses.KEY_RIGHT:
                        if current_record < len(records) and current_col < len(records[current_record][1]) - 1:
                            current_col += 1
                    elif key == curses.KEY_PPAGE:
                        current_record = max(0, current_record - visible_records)
                        current_col = 0
                    elif key == curses.KEY_NPAGE:
                        current_record = min(len(records) - 1, current_record + visible_records)
                        current_col = 0
                    elif key == curses.KEY_HOME:
                        current_record = 0
                        current_col = 0
                    elif key == curses.KEY_END:
                        current_record = len(records) - 1
                        current_col = 0
                        
            except curses.error:
                # Handle curses errors gracefully
                pass
    
    # Check if we're in a web environment (no TTY) or proper terminal
    is_web_environment = not (os.isatty(sys.stdin.fileno()) and os.isatty(sys.stdout.fileno()))
    
    if not is_web_environment:
        try:
            os.environ['TERM'] = 'xterm-256color'  # Ensure proper terminal type
            curses.wrapper(browser_main)
            print("\n[INFO] EDTFILE browser terminated")
            return
        except Exception as e:
            print(f"\n[WARNING] Curses browser failed: {e}")
    
    # Use web-friendly display mode for OpenASP AX terminal
    print("[INFO] Using web-compatible display mode...")
    _display_records_web_mode(records, raw_records, file_path, rectype, reclen, encoding)

def _safe_print(text, end="\n"):
    """Safe print function that handles encoding issues"""
    try:
        print(text, end=end)
    except UnicodeEncodeError:
        # If print fails, try with UTF-8 encoding
        try:
            sys.stdout.buffer.write(text.encode('utf-8', errors='replace'))
            sys.stdout.buffer.write(end.encode('utf-8'))
            sys.stdout.flush()
        except:
            # Final fallback to ASCII-safe representation
            safe_text = text.encode('ascii', errors='replace').decode('ascii')
            print(safe_text, end=end)

def _display_records_web_mode(records, raw_records, file_path, rectype, reclen, encoding):
    """Web-compatible display mode for OpenASP AX terminal"""
    _safe_print("=" * 80)
    _safe_print(f"EDTFILE Dataset Browser - {os.path.basename(file_path)}")
    _safe_print(f"Type: {rectype} | RecLen: {reclen} | Encoding: {encoding} | Records: {len(records)}")
    _safe_print("=" * 80)
    _safe_print("")
    
    # Display all records with hex information
    # For web interface, always convert SJIS to Unicode for proper display
    for i, (record_num, record_str) in enumerate(records):
        # Web interface always needs Unicode conversion from SJIS
        if i < len(raw_records):
            try:
                # Force SJIS to Unicode conversion for web display - keep full record length
                web_display_str = _convert_bytes_to_string(raw_records[i], 'shift_jis')
            except Exception as e:
                web_display_str = record_str  # Fallback to original
        else:
            web_display_str = record_str
            
        _safe_print(f"Record {record_num:5}: {web_display_str}")
        
        # Show hex dump for each record (first 64 bytes)
        if i < len(raw_records):
            raw_data = raw_records[i]
            _safe_print(f"         HEX: ", end="")
            
            # Show first 32 bytes in hex
            hex_display = ""
            for j, byte_val in enumerate(raw_data[:32]):
                hex_display += f"{byte_val:02X} "
                if j == 15:  # Add break after 16 bytes
                    hex_display += "\n              "
            
            _safe_print(hex_display.rstrip())
            
            # Show character representation
            char_display = ""
            for byte_val in raw_data[:32]:
                char_display += chr(byte_val) if 32 <= byte_val <= 126 else '.'
                if len(char_display) == 16:
                    char_display += "\n              "
            
            _safe_print(f"         CHR: {char_display.rstrip()}")
            
            if len(raw_data) > 32:
                _safe_print(f"         ... ({len(raw_data)} total bytes)")
        
        _safe_print("-" * 80)
    
    _safe_print(f"\nTotal Records: {len(records)}")
    _safe_print(f"File: {file_path}")
    _safe_print("=" * 80)

def _display_records_with_hex(records, raw_records, file_path, rectype, reclen, encoding):
    """Enhanced display mode with hex viewer capabilities"""
    current_record = 0
    current_col = 0
    
    # Save terminal settings
    old_settings = None
    try:
        old_settings = termios.tcgetattr(sys.stdin)
        tty.setraw(sys.stdin.fileno())
    except:
        pass
    
    def draw_screen():
        """Draw the complete screen"""
        os.system('clear')
        print(f"EDTFILE Dataset Browser - {os.path.basename(file_path)}")
        print(f"Type: {rectype} | RecLen: {reclen} | Encoding: {encoding}")
        print(f"Record {current_record + 1}/{len(records)} | Column: {current_col + 1}")
        print("=" * 80)
        
        # Display current record
        if current_record < len(records):
            record_num, record_str = records[current_record]
            print(f"Record {record_num:5}: {record_str}")
            print()
            
            # Show hex dump of current record
            if current_record < len(raw_records):
                raw_data = raw_records[current_record]
                print("HEX VIEW:")
                print("-" * 80)
                
                # Show hex dump in 16-byte lines
                for offset in range(0, len(raw_data), 16):
                    hex_part = ""
                    char_part = ""
                    
                    for i in range(16):
                        pos = offset + i
                        if pos < len(raw_data):
                            byte_val = raw_data[pos]
                            if pos == current_col:
                                hex_part += f"[{byte_val:02X}]"
                                char_part += f"[{chr(byte_val) if 32 <= byte_val <= 126 else '.'}]"
                            else:
                                hex_part += f"{byte_val:02X} "
                                char_part += chr(byte_val) if 32 <= byte_val <= 126 else '.'
                        else:
                            hex_part += "   "
                            char_part += " "
                        
                        if i == 7:  # Add extra space in middle
                            hex_part += " "
                    
                    print(f"{offset:04X}: {hex_part} | {char_part}")
                
                print("-" * 80)
                
                # Show current byte info
                if current_col < len(raw_data):
                    byte_val = raw_data[current_col]
                    print(f"Cursor Position: {current_col:04X} (decimal {current_col})")
                    print(f"Byte Value: 0x{byte_val:02X} (decimal {byte_val})")
                    print(f"Character: '{chr(byte_val) if 32 <= byte_val <= 126 else '.'}'")
        
        print("=" * 80)
        print("Commands: [↑↓] Records | [←→] Columns | [P]age | [H]ex | [Q]uit")
        print("          [N]ext | [F]irst | [L]ast | [G]oto")
        print("Enter command: ", end="", flush=True)

    # Initial screen draw
    draw_screen()
    
    try:
        while True:
            # Read single character without redrawing screen
            if old_settings:
                key = sys.stdin.read(1)
                if not key:  # Handle EOF
                    break
                key = key.upper()
            else:
                key = input().upper()
            
            screen_needs_redraw = True
            
            if key == 'Q':
                break
            elif key == 'N' or key == ' ':
                if current_record < len(records) - 1:
                    current_record += 1
                    current_col = 0
            elif key == 'P':
                if current_record > 0:
                    current_record -= 1
                    current_col = 0
            elif key == 'F':
                current_record = 0
                current_col = 0
            elif key == 'L':
                current_record = len(records) - 1
                current_col = 0
            elif key == '\x1b':  # ESC sequence for arrow keys
                if old_settings:
                    # Read the rest of the escape sequence
                    seq = sys.stdin.read(2)
                    if seq == '[A':  # Up arrow
                        if current_record > 0:
                            current_record -= 1
                            current_col = 0
                    elif seq == '[B':  # Down arrow
                        if current_record < len(records) - 1:
                            current_record += 1
                            current_col = 0
                    elif seq == '[D':  # Left arrow
                        if current_col > 0:
                            current_col -= 1
                    elif seq == '[C':  # Right arrow
                        if current_record < len(raw_records) and current_col < len(raw_records[current_record]) - 1:
                            current_col += 1
                    else:
                        screen_needs_redraw = False  # Unknown escape sequence
                else:
                    screen_needs_redraw = False
            elif key == 'G':
                print("\nGoto record number (1-{}): ".format(len(records)), end="", flush=True)
                if old_settings:
                    termios.tcsetattr(sys.stdin, termios.TCSADRAIN, old_settings)
                try:
                    line_num = int(input())
                    if 1 <= line_num <= len(records):
                        current_record = line_num - 1
                        current_col = 0
                except:
                    pass
                if old_settings:
                    tty.setraw(sys.stdin.fileno())
            elif key == 'H':
                # Show help
                os.system('clear')
                print("EDTFILE Dataset Browser - Help")
                print("=" * 50)
                print("Navigation:")
                print("  ↑/↓ or N/P    : Move between records")
                print("  ←/→           : Move cursor left/right in record")
                print("  F             : First record")
                print("  L             : Last record")
                print("  G             : Goto specific record number")
                print("  Q             : Quit")
                print("")
                print("Display:")
                print("  - Current record is shown with hex dump")
                print("  - Current byte position shown with [] brackets")
                print("  - Cursor position and byte value displayed below")
                print("")
                print("Press any key to continue...")
                if old_settings:
                    sys.stdin.read(1)
                else:
                    input()
            else:
                screen_needs_redraw = False  # Unknown key, don't redraw
            
            # Only redraw screen if something changed
            if screen_needs_redraw:
                draw_screen()
    
    finally:
        # Restore terminal settings
        if old_settings:
            termios.tcsetattr(sys.stdin, termios.TCSADRAIN, old_settings)
        
        os.system('clear')
        print(f"[INFO] EDTFILE browser terminated")

def CTTFILE(command):
    """
    Fujitsu ASP CTTFILE command implementation
    Format: CTTFILE INFILE(LIB/FILE),OUTFILE(LIB/FILE),VOL-VOLUME,MODE=COPY|REPL|CONV[,additional params]
    """
    # Parse command parameters
    try:
        parts = command.replace('CTTFILE ', '').split(',')
        params = {}
        
        for part in parts:
            if part.startswith('INFILE('):
                params['INFILE'] = part[7:-1]  # Remove INFILE( and )
            elif part.startswith('OUTFILE('):
                params['OUTFILE'] = part[8:-1]  # Remove OUTFILE( and )
            elif '=' in part:
                key, value = part.split('=', 1)
                params[key.strip()] = value.strip()
            elif '-' in part:
                key, value = part.split('-', 1)
                params[key.strip()] = value.strip()
    except Exception as e:
        print(f"[ERROR] Invalid command format: {e}")
        print("[USAGE] CTTFILE INFILE(LIB/FILE),OUTFILE(LIB/FILE),VOL-VOLUME,MODE=COPY|REPL|CONV")
        return
    
    # Validate required parameters
    if not params.get('INFILE') or not params.get('OUTFILE'):
        print("[ERROR] INFILE and OUTFILE parameters are required")
        return
    
    if not params.get('VOL'):
        print("[ERROR] VOL parameter is required")
        return
    
    mode = params.get('MODE', 'COPY').upper()
    if mode not in ['COPY', 'REPL', 'CONV']:
        print("[ERROR] MODE must be COPY, REPL, or CONV")
        return
    
    # Parse input and output file paths
    try:
        in_lib, in_file = params['INFILE'].split('/', 1)
        out_lib, out_file = params['OUTFILE'].split('/', 1)
    except:
        print("[ERROR] File specification must be in LIB/FILE format")
        return
    
    volume = params['VOL']
    
    # Construct full paths
    infile_path = os.path.join(VOLUME_ROOT, volume, in_lib, in_file)
    outfile_path = os.path.join(VOLUME_ROOT, volume, out_lib, out_file)
    
    # Check input file exists
    if not os.path.isfile(infile_path):
        print(f"[ERROR] Input file '{in_file}' does not exist in library '{in_lib}' on volume '{volume}'")
        return
    
    # Ensure output directory exists
    out_dir = os.path.dirname(outfile_path)
    if not os.path.exists(out_dir):
        print(f"[ERROR] Output library '{out_lib}' does not exist on volume '{volume}'")
        return
    
    # Get file information from catalog
    file_info = get_file_info(volume, in_file)
    rectype = file_info.get('RECTYPE', 'FB')
    reclen = file_info.get('RECLEN', 80)
    
    print(f"[INFO] CTTFILE - Mode: {mode}")
    print(f"[INFO] Input: {in_lib}/{in_file} (RECTYPE={rectype}, RECLEN={reclen})")
    print(f"[INFO] Output: {out_lib}/{out_file}")
    
    try:
        if mode == 'COPY':
            _cttfile_copy(infile_path, outfile_path, rectype, reclen)
        elif mode == 'REPL':
            find = params.get('FIND', '')
            replace = params.get('REPLACE', '')
            if not find:
                print("[ERROR] FIND parameter is required for REPL mode")
                return
            _cttfile_replace(infile_path, outfile_path, find, replace, rectype, reclen)
        elif mode == 'CONV':
            inenc = params.get('INENC', 'utf-8')
            outenc = params.get('OUTENC', 'utf-8')
            _cttfile_convert(infile_path, outfile_path, inenc, outenc, rectype, reclen)
        
        # Update catalog for output file
        update_catalog_info(volume, out_file, rectype=rectype, reclen=reclen)
        print(f"[INFO] CTTFILE completed successfully")
        
    except Exception as e:
        print(f"[ERROR] CTTFILE failed: {e}")

def _cttfile_copy(infile, outfile, rectype, reclen):
    """Copy mode - simple file copy"""
    shutil.copy2(infile, outfile)
    print(f"[INFO] File copied successfully")

def _cttfile_replace(infile, outfile, find_str, replace_str, rectype, reclen):
    """Replace mode - find and replace in records"""
    with open(infile, 'rb') as inf:
        content = inf.read()
    
    if rectype == 'FB':
        # Fixed block records
        records = []
        pos = 0
        while pos < len(content):
            record = content[pos:pos+reclen]
            if len(record) == 0:
                break
            # Decode, replace, encode
            try:
                record_str = record.decode('utf-8', errors='replace')
                record_str = record_str.replace(find_str, replace_str)
                # Ensure fixed length
                if len(record_str.encode('utf-8')) > reclen:
                    record_str = record_str[:reclen]
                record_bytes = record_str.encode('utf-8').ljust(reclen, b' ')
                records.append(record_bytes)
            except:
                records.append(record)  # Keep original if decode fails
            pos += reclen
        
        # Write output
        with open(outfile, 'wb') as outf:
            for record in records:
                outf.write(record)
    
    elif rectype == 'VB':
        # Variable block with RDW
        records = []
        pos = 0
        while pos < len(content):
            if pos + 4 > len(content):
                break
            # Read RDW (4 bytes)
            rdw = content[pos:pos+4]
            rec_len = int.from_bytes(rdw[:2], 'big')
            if rec_len == 0 or pos + rec_len > len(content):
                break
            
            record = content[pos+4:pos+rec_len]
            try:
                record_str = record.decode('utf-8', errors='replace')
                record_str = record_str.replace(find_str, replace_str)
                record_bytes = record_str.encode('utf-8')
                # Update RDW with new length
                new_len = len(record_bytes) + 4
                new_rdw = new_len.to_bytes(2, 'big') + rdw[2:]
                records.append(new_rdw + record_bytes)
            except:
                records.append(content[pos:pos+rec_len])
            pos += rec_len
        
        # Write output
        with open(outfile, 'wb') as outf:
            for record in records:
                outf.write(record)
    
    else:  # LB - Line block with newlines
        # Simple text file processing
        with open(infile, 'r', encoding='utf-8', errors='replace') as inf:
            content = inf.read()
        content = content.replace(find_str, replace_str)
        with open(outfile, 'w', encoding='utf-8') as outf:
            outf.write(content)
    
    print(f"[INFO] Replaced '{find_str}' with '{replace_str}'")

def _cttfile_convert(infile, outfile, inenc, outenc, rectype, reclen):
    """Convert mode - character encoding conversion"""
    try:
        if rectype == 'FB':
            # Fixed block records
            with open(infile, 'rb') as inf:
                content = inf.read()
            
            records = []
            pos = 0
            while pos < len(content):
                record = content[pos:pos+reclen]
                if len(record) == 0:
                    break
                # Decode and encode
                try:
                    record_str = record.decode(inenc, errors='replace')
                    record_bytes = record_str.encode(outenc, errors='replace')
                    # Ensure fixed length
                    if len(record_bytes) > reclen:
                        record_bytes = record_bytes[:reclen]
                    else:
                        record_bytes = record_bytes.ljust(reclen, b' ')
                    records.append(record_bytes)
                except Exception as e:
                    print(f"[WARNING] Record at position {pos} conversion failed: {e}")
                    records.append(record)  # Keep original
                pos += reclen
            
            # Write output
            with open(outfile, 'wb') as outf:
                for record in records:
                    outf.write(record)
        
        elif rectype == 'VB':
            # Variable block with RDW
            with open(infile, 'rb') as inf:
                content = inf.read()
            
            records = []
            pos = 0
            while pos < len(content):
                if pos + 4 > len(content):
                    break
                # Read RDW
                rdw = content[pos:pos+4]
                rec_len = int.from_bytes(rdw[:2], 'big')
                if rec_len == 0 or pos + rec_len > len(content):
                    break
                
                record = content[pos+4:pos+rec_len]
                try:
                    record_str = record.decode(inenc, errors='replace')
                    record_bytes = record_str.encode(outenc, errors='replace')
                    # Update RDW with new length
                    new_len = len(record_bytes) + 4
                    new_rdw = new_len.to_bytes(2, 'big') + rdw[2:]
                    records.append(new_rdw + record_bytes)
                except Exception as e:
                    print(f"[WARNING] Record at position {pos} conversion failed: {e}")
                    records.append(content[pos:pos+rec_len])
                pos += rec_len
            
            # Write output
            with open(outfile, 'wb') as outf:
                for record in records:
                    outf.write(record)
        
        else:  # LB - Line block
            # Simple text file conversion
            with open(infile, 'r', encoding=inenc, errors='replace') as inf:
                content = inf.read()
            with open(outfile, 'w', encoding=outenc) as outf:
                outf.write(content)
        
        print(f"[INFO] Converted from {inenc} to {outenc}")
        
    except Exception as e:
        print(f"[ERROR] Encoding conversion failed: {e}")

def HELP(command=None):
    """Display help information for ASP system commands"""
    
    if command:
        # Show specific command help
        command = command.upper()
        specific_help = get_specific_command_help(command)
        if specific_help:
            print(specific_help)
            return
        else:
            print(f"[ERROR] No help available for command: {command}")
            print("Use HELP without parameters to see all available commands.")
            return
    
    # Show general help
    help_text = """
================================================================================
                        ASP SYSTEM COMMAND REFERENCE MANUAL
================================================================================

OVERVIEW:
The ASP (Application System Platform) Command Terminal provides a comprehensive
set of commands for managing libraries, files, programs, and system resources.
All commands follow Fujitsu ASP standard syntax and conventions.

COMMAND CATEGORIES:

┌─ LIBRARY MANAGEMENT ──────────────────────────────────────────────────────┐
│ CRTLIB  - Create a new library in specified volume                        │
│ DLTLIB  - Delete an existing library                                      │
│ WRKLIB  - Display library contents and information                        │
└────────────────────────────────────────────────────────────────────────────┘

┌─ FILE MANAGEMENT ─────────────────────────────────────────────────────────┐
│ CRTFILE - Create a new file with specified attributes                     │
│ DLTFILE - Delete an existing file                                         │
│ DSPFD   - Display file description and attributes                         │
│ EDTFILE - Interactive file editor with multiple modes                     │
└────────────────────────────────────────────────────────────────────────────┘

┌─ PROGRAM & MAP MANAGEMENT ────────────────────────────────────────────────┐
│ CALL    - Execute programs (JAVA/COBOL/SHELL) with SMED map support       │
│ CRTPGM  - Create program objects in catalog                               │
│ CRTMAP  - Create screen map objects for SMED displays                     │
└────────────────────────────────────────────────────────────────────────────┘

┌─ SYSTEM INFORMATION ──────────────────────────────────────────────────────┐
│ WRKVOL  - Display volume information and usage statistics                 │
│ WRKOBJ  - Display objects by type (FILE/LIB/PGM/MAP)                      │
│ DSPJOB  - Display job information and status                              │
└────────────────────────────────────────────────────────────────────────────┘

┌─ ENCODING & CONVERSION ───────────────────────────────────────────────────┐
│ CTTFILE - Convert file encoding (SJIS ↔ UTF-8 ↔ ASCII)                   │
└────────────────────────────────────────────────────────────────────────────┘

┌─ MESSAGING SYSTEM ────────────────────────────────────────────────────────┐
│ SNDMSG  - Send messages to users                                          │
│ RCVMSG  - Receive messages from queue                                     │
│ WRKMSG  - Work with message queue                                         │
└────────────────────────────────────────────────────────────────────────────┘

┌─ BACKUP & RESTORE ────────────────────────────────────────────────────────┐
│ SAVLIB  - Save library to backup file                                     │
│ RSTLIB  - Restore library from backup file                                │
└────────────────────────────────────────────────────────────────────────────┘

SYNTAX CONVENTIONS:
• Commands are case-insensitive
• Parameters are separated by commas (,)
• Parameter format: KEYWORD-VALUE
• Optional parameters are shown in [brackets]
• Required parameters are shown in <angle brackets>
• File paths use library/filename format
• Quotes required for parameters with spaces
• Japanese SJIS encoding fully supported

SPECIAL FEATURES:
• Full-width Japanese character support in SMED maps
• Interactive web terminal with 24x80 grid display
• Real-time SMED map rendering with field input/output
• Automatic program type detection (JAVA/COBOL/SHELL)
• @PGMEC variable for program execution status tracking
• Hierarchical catalog system (VOLUME→LIBRARY→OBJECT)

EXAMPLES:
  HELP CALL                    - Show detailed help for CALL command
  CALL PGM-TestProgram.TESTLIB - Execute Java program with SMED display
  EDTFILE FILE(TESTLIB/DATA),VOL-DISK01,MODE-BROWSE
  CTTFILE INFILE-jp.sjis,OUTFILE-jp.utf8,INENC-shift_jis,OUTENC-utf-8

For specific command syntax, use: HELP <command>
For system administration guide, contact your system administrator.

================================================================================
"""
    print(help_text)

def get_specific_command_help(command):
    """Get detailed help for a specific command"""
    
    help_dict = {
        'CALL': """
================================================================================
                                CALL COMMAND HELP
================================================================================

PURPOSE:
Execute programs (JAVA, COBOL, SHELL) with optional parameters and SMED map 
display support. Follows Fujitsu ASP standard command format.

SYNTAX:
CALL PGM-<program>[.<library>][,PARA-(<parameters>)][,VOL-<volume>]

PARAMETERS:
• PGM-<program>    : Program name (required)
• .<library>       : Library name (optional, auto-searched if omitted)
• PARA-(<params>)  : Parameters in parentheses (optional)
• VOL-<volume>     : Volume name (optional, for library search)

SUPPORTED PROGRAM TYPES:
• JAVA   : JAR files with JSON parameter support
• COBOL  : Compiled programs with standard linkage
• SHELL  : Shell scripts with environment variables

SMED MAP INTEGRATION:
• Automatic SMED map detection and display
• 24x80 terminal grid rendering in web interface
• Interactive field input/output with full-width character support
• Tab navigation and Enter submission

SYSTEM VARIABLES:
• @PGMEC : Program execution code (0=success, non-zero=error)

EXAMPLES:
CALL PGM-TestProgram.TESTLIB
CALL PGM-PAYROLL.HRLIB,PARA-(month=12,year=2024),VOL-DISK01
CALL PGM-BATCH001,PARA-(debug=true,verbose=on)

NOTES:
• Programs must exist in catalog.json
• SMED maps only displayed in web terminal
• Parameters passed as JSON to JAVA programs
• Library search performed across all volumes if not specified
        """,
        
        'EDTFILE': """
================================================================================
                               EDTFILE COMMAND HELP
================================================================================

PURPOSE:
Interactive file editor supporting multiple record types and display modes.
Provides both CLI curses interface and web browser interface.

SYNTAX:
EDTFILE FILE(<library>/<filename>),VOL-<volume>[,MODE-<mode>]

PARAMETERS:
• FILE(<lib>/<file>) : File specification (required)
• VOL-<volume>       : Volume name (required)
• MODE-<mode>        : Display mode (optional)

MODES:
• BROWSE   : Read-only curses browser (default)
• DISPLAY  : Web browser interface
• EDIT     : Interactive editing mode

SUPPORTED RECORD TYPES:
• FB (Fixed Block)   : Fixed-length records
• VB (Variable Block): Variable-length records with length prefix
• LB (Line Block)    : Line-based records with line terminators

FEATURES:
• Hexadecimal display toggle (:hexon/:hexoff)
• Record-by-record navigation
• SJIS encoding support for Japanese text
• Cursor positioning and field highlighting
• F1 help, F3 exit, navigation keys

EXAMPLES:
EDTFILE FILE(TESTLIB/EMPLOYEE),VOL-DISK01
EDTFILE FILE(DATALIB/SALES),VOL-DISK01,MODE-DISPLAY
EDTFILE FILE(LOGLIB/AUDIT),VOL-DISK99,MODE-BROWSE

KEYBOARD SHORTCUTS:
• Arrow Keys : Navigate records/fields
• F1         : Show help
• F3         : Exit editor
• :hexon     : Enable hex display
• :hexoff    : Disable hex display
        """,
        
        'CTTFILE': """
================================================================================
                              CTTFILE COMMAND HELP
================================================================================

PURPOSE:
Convert file encoding between different character sets with support for
Japanese SJIS, UTF-8, ASCII, and other international encodings.

SYNTAX:
CTTFILE INFILE-<input>,OUTFILE-<output>,INENC-<input_encoding>,OUTENC-<output_encoding>

PARAMETERS:
• INFILE-<input>    : Input file path (required)
• OUTFILE-<output>  : Output file path (required)
• INENC-<encoding>  : Input file encoding (required)
• OUTENC-<encoding> : Output file encoding (required)

SUPPORTED ENCODINGS:
• shift_jis  : Japanese Shift-JIS (SJIS)
• utf-8      : Unicode UTF-8
• ascii      : ASCII (7-bit)
• cp932      : Windows Japanese
• euc-jp     : Extended Unix Code Japan
• iso-2022-jp: ISO-2022 Japanese

FEATURES:
• Automatic encoding detection fallback
• Error handling with graceful degradation
• Preservation of file structure
• Support for large files
• Validation of encoding compatibility

EXAMPLES:
CTTFILE INFILE-japanese.sjis,OUTFILE-japanese.utf8,INENC-shift_jis,OUTENC-utf-8
CTTFILE INFILE-data.txt,OUTFILE-data.ascii,INENC-utf-8,OUTENC-ascii
CTTFILE INFILE-legacy.cp932,OUTFILE-modern.utf8,INENC-cp932,OUTENC-utf-8

NOTES:
• Files are processed entirely in memory
• Original files are preserved
• Output directories must exist
• Character mapping may cause data loss between incompatible encodings
        """,
        
        'CRTLIB': """
================================================================================
                               CRTLIB COMMAND HELP
================================================================================

PURPOSE:
Create a new library directory structure in the specified volume with proper
initialization for ASP system usage.

SYNTAX:
CRTLIB LIB-<library>,VOL-<volume>

PARAMETERS:
• LIB-<library> : Library name (required, alphanumeric)
• VOL-<volume>  : Volume name (required)

FEATURES:
• Creates physical directory structure
• Updates catalog.json with new library entry
• Sets proper permissions and attributes
• Validates library name conventions
• Checks for existing library conflicts

EXAMPLES:
CRTLIB LIB-PAYROLL,VOL-DISK01
CRTLIB LIB-TESTLIB,VOL-DISK99

NOTES:
• Library names must be valid directory names
• Volume must exist before library creation
• Library creation is logged in system audit trail
        """,

        'WRKOBJ': """
================================================================================
                               WRKOBJ COMMAND HELP
================================================================================

PURPOSE:
Display and manage objects of specified types within the ASP catalog system.
Supports filtering by type, volume, and library.

SYNTAX:
WRKOBJ TYPE-<type>,VOL-<volume>[,LIB-<library>]

PARAMETERS:
• TYPE-<type>    : Object type (required: FILE, LIB, PGM, MAP, DATASET)
• VOL-<volume>   : Volume name (required)
• LIB-<library>  : Library filter (optional)

OBJECT TYPES:
• FILE    : Data files and datasets
• LIB     : Library directories
• PGM     : Program objects (JAVA/COBOL/SHELL)
• MAP     : Screen map definitions (SMED)
• DATASET : Structured data collections

DISPLAY FORMAT:
Shows hierarchical object structure with attributes:
• Object name and type
• Size and creation date
• Permissions and access rights
• Associated metadata

EXAMPLES:
WRKOBJ TYPE-FILE,VOL-DISK01,LIB-TESTLIB
WRKOBJ TYPE-PGM,VOL-DISK01
WRKOBJ TYPE-MAP,VOL-DISK99,LIB-MENULIB

NOTES:
• Results sorted by object name
• Large libraries may have paginated output
• Requires read permissions on target volume/library
        """
    }
    
    return help_dict.get(command)

def CHGLIBL(command_line):
    """
    CHGLIBL - Change Library List
    Updates the library list for common part, object-specific part, or file-specific part.
    
    Syntax: CHGLIBL [LIBL=lib1,lib2,...] [OBJL=lib1,lib2,...] [FILEL=lib1,lib2,...]
                    [MODE={@ADD|@DLT}] [POSITION={@TOP|@BOTTOM|number}] [ERRMODE={@YES|@NO}]
    """
    try:
        print(f"[DEBUG] CHGLIBL command: {command_line}")
        
        # Parse parameters from command line
        parts = command_line.split(maxsplit=1)
        if len(parts) < 2:
            print("[ERROR] CHGLIBL: Parameters required")
            set_pgmec(820)
            return False
        
        param_string = parts[1]
        params = _parse_parameters(param_string)
        
        # Get current context (initialize if not exists)
        context = _get_library_context()
        
        # Extract parameters with defaults
        mode = params.get('MODE', '@ADD').upper()
        position = params.get('POSITION', '@BOTTOM').upper()
        errmode = params.get('ERRMODE', '@NO').upper()
        
        # Validate MODE parameter
        if mode not in ['@ADD', '@DLT']:
            print(f"[ERROR] CHGLIBL: Invalid MODE '{mode}'. Valid values: @ADD, @DLT")
            set_pgmec(820)
            return False
        
        # Validate ERRMODE parameter
        if errmode not in ['@YES', '@NO']:
            print(f"[ERROR] CHGLIBL: Invalid ERRMODE '{errmode}'. Valid values: @YES, @NO")
            set_pgmec(820)
            return False
        
        # Process library list parameters
        success = True
        
        # Process LIBL (Common Library List)
        if 'LIBL' in params:
            libl_value = params['LIBL']
            if libl_value == '@NONE':
                # Clear the library list
                context['LIBL'] = []
                print("[INFO] CHGLIBL: Common library list cleared")
            else:
                success &= _process_library_list(
                    context, 'LIBL', libl_value, mode, position, errmode, max_libs=24
                )
        
        # Process OBJL (Object-specific Library List)
        if 'OBJL' in params:
            objl_value = params['OBJL']
            if objl_value == '@NONE':
                # Clear the object library list
                context['OBJL'] = []
                print("[INFO] CHGLIBL: Object library list cleared")
            else:
                success &= _process_library_list(
                    context, 'OBJL', objl_value, mode, position, errmode, max_libs=16
                )
        
        # Process FILEL (File-specific Library List)
        if 'FILEL' in params:
            filel_value = params['FILEL']
            if filel_value == '@NONE':
                # Clear the file library list
                context['FILEL'] = []
                print("[INFO] CHGLIBL: File library list cleared")
            else:
                success &= _process_library_list(
                    context, 'FILEL', filel_value, mode, position, errmode, max_libs=16
                )
        
        # Save updated context
        if success:
            _save_library_context(context)
            print("[INFO] CHGLIBL: Library list updated successfully")
            reset_pgmec()
        else:
            print("[ERROR] CHGLIBL: Failed to update library list")
            set_pgmec(999)
        
        return success
        
    except Exception as e:
        print(f"[ERROR] CHGLIBL execution error: {e}")
        set_pgmec(999)
        return False

def _get_library_context():
    """Get current library context from profile or initialize default"""
    try:
        context_file = os.path.join(PROFILE_DIR, "library_context.json")
        if os.path.exists(context_file):
            with open(context_file, 'r', encoding='utf-8') as f:
                return json.load(f)
    except Exception as e:
        print(f"[DEBUG] Could not load library context: {e}")
    
    # Return default context
    return {
        'LIBL': [],   # Common library list
        'OBJL': [],   # Object-specific library list  
        'FILEL': []   # File-specific library list
    }

def _save_library_context(context):
    """Save library context to profile file"""
    try:
        context_file = os.path.join(PROFILE_DIR, "library_context.json")
        with open(context_file, 'w', encoding='utf-8') as f:
            json.dump(context, f, indent=2)
        return True
    except Exception as e:
        print(f"[ERROR] Could not save library context: {e}")
        return False

def _process_library_list(context, list_type, libraries_str, mode, position, errmode, max_libs):
    """Process library list changes for LIBL, OBJL, or FILEL"""
    try:
        # Parse library names
        library_names = [lib.strip() for lib in libraries_str.split(',') if lib.strip()]
        
        # Get current list
        current_list = context.get(list_type, [])
        
        if mode == '@ADD':
            return _add_libraries_to_list(
                current_list, library_names, position, errmode, max_libs, list_type
            )
        elif mode == '@DLT':
            return _remove_libraries_from_list(
                current_list, library_names, errmode, list_type
            )
        
        return False
        
    except Exception as e:
        print(f"[ERROR] Error processing {list_type}: {e}")
        return False

def _add_libraries_to_list(current_list, library_names, position, errmode, max_libs, list_type):
    """Add libraries to the specified list"""
    try:
        # Check if libraries already exist (for @ADD mode)
        for lib_name in library_names:
            if lib_name in current_list:
                print(f"[ERROR] CHGLIBL: Library '{lib_name}' already exists in {list_type}")
                set_pgmec(520)  # S0520 error - library already exists
                return False
        
        # Validate library existence if ERRMODE=@YES
        if errmode == '@YES':
            catalog = get_catalog_info()
            for lib_name in library_names:
                lib_found = False
                for volume in catalog.values():
                    if lib_name in volume:
                        lib_found = True
                        break
                if not lib_found:
                    print(f"[ERROR] CHGLIBL: Library '{lib_name}' not found")
                    set_pgmec(999)
                    return False
        
        # Check maximum library limit
        if len(current_list) + len(library_names) > max_libs:
            print(f"[ERROR] CHGLIBL: Maximum {max_libs} libraries allowed for {list_type}")
            set_pgmec(999)
            return False
        
        # Add libraries based on position
        if position == '@TOP':
            # Add at the beginning in reverse order to maintain specified order
            for lib_name in reversed(library_names):
                current_list.insert(0, lib_name)
        elif position == '@BOTTOM':
            # Add at the end
            current_list.extend(library_names)
        elif position.isdigit():
            # Insert at specific position
            insert_pos = int(position)
            if insert_pos < 0 or insert_pos > len(current_list):
                print(f"[ERROR] CHGLIBL: Invalid position {insert_pos}")
                set_pgmec(820)
                return False
            # Insert libraries in order
            for i, lib_name in enumerate(library_names):
                current_list.insert(insert_pos + i, lib_name)
        else:
            print(f"[ERROR] CHGLIBL: Invalid POSITION '{position}'")
            set_pgmec(820)
            return False
        
        print(f"[INFO] CHGLIBL: Added {len(library_names)} libraries to {list_type}")
        for lib_name in library_names:
            print(f"[INFO] CHGLIBL: + {lib_name}")
        
        return True
        
    except Exception as e:
        print(f"[ERROR] Error adding libraries to {list_type}: {e}")
        return False

def _remove_libraries_from_list(current_list, library_names, errmode, list_type):
    """Remove libraries from the specified list"""
    try:
        removed_count = 0
        
        for lib_name in library_names:
            if lib_name in current_list:
                current_list.remove(lib_name)
                removed_count += 1
                print(f"[INFO] CHGLIBL: - {lib_name}")
            else:
                # For @DLT mode, non-existent libraries are ignored (as per spec)
                print(f"[DEBUG] CHGLIBL: Library '{lib_name}' not in {list_type} (ignored)")
        
        print(f"[INFO] CHGLIBL: Removed {removed_count} libraries from {list_type}")
        return True
        
    except Exception as e:
        print(f"[ERROR] Error removing libraries from {list_type}: {e}")
        return False
