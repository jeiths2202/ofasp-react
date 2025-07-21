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
CATALOG_FILE = os.path.join(CONFIG_ROOT, "catalog.json")
JOB_LOG_DIR = os.path.join(VOLUME_ROOT, "JOBLOG")

# Initialize configuration directories
os.makedirs(CONFIG_ROOT, exist_ok=True)
os.makedirs(PROFILE_DIR, exist_ok=True)
os.makedirs(JOB_LOG_DIR, exist_ok=True)

def get_catalog_info():
    """Get file information from centralized catalog.json"""
    if os.path.exists(CATALOG_FILE):
        try:
            with open(CATALOG_FILE, 'r', encoding='utf-8') as f:
                return json.load(f)
        except:
            pass
    return {}

def update_catalog_info(volume, filename, rectype="FB", reclen=80, **kwargs):
    """Update file information in catalog.json"""
    catalog = get_catalog_info()
    
    if volume not in catalog:
        catalog[volume] = {}
    
    if filename not in catalog[volume]:
        catalog[volume][filename] = {}
    
    catalog[volume][filename]["RECTYPE"] = rectype
    catalog[volume][filename]["RECLEN"] = reclen
    
    # Update additional attributes
    for key, value in kwargs.items():
        catalog[volume][filename][key] = value
    
    try:
        with open(CATALOG_FILE, 'w', encoding='utf-8') as f:
            json.dump(catalog, f, indent=2, ensure_ascii=False)
    except Exception as e:
        print(f"[WARNING] catalog.json update failed: {e}")

def get_file_info(volume, filename):
    """Get catalog information for a specific file"""
    catalog = get_catalog_info()
    return catalog.get(volume, {}).get(filename, {
        "RECTYPE": "FB",
        "RECLEN": 80
    })

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
    # Example: WRKOBJ LIB-SALES,VOL-DISK01
    params = dict(item.split('-') for item in command.replace('WRKOBJ ', '').split(','))
    lib = params.get('LIB')
    vol = params.get('VOL')

    if not lib or not vol:
        print("[ERROR] LIB or VOL parameter is missing.")
        return

    lib_path = os.path.join(VOLUME_ROOT, vol, lib)

    if not os.path.isdir(lib_path):
        print(f"[ERROR] Library '{lib}'does not exist in volume '{vol}'.")
        return

    files = os.listdir(lib_path)
    if not files:
        print(f"[INFO] Library '{lib}'has no objects.")
        return

    print(f"[INFO] Library '{lib}' object list (volume: {vol}):")
    for f in files:
        f_path = os.path.join(lib_path, f)
        size = os.path.getsize(f_path)
        mtime = datetime.fromtimestamp(os.path.getmtime(f_path)).strftime('%Y-%m-%d %H:%M:%S')
        print(f"  [FILE] {f.ljust(20)} | Size: {str(size).rjust(6)} Byte | Modified: {mtime}")

def DSPFD(command):
    # Example: DSPFD FILE(SALES/REPORT),VOL-DISK01
    main_part, *others = command.replace('DSPFD ', '').split(',')
    file_lib, file_name = main_part.replace('FILE(', '').replace(')', '').split('/')
    params = dict(item.split('-') for item in others if '-' in item)
    vol = params.get('VOL')

    if not vol:
        print("[ERROR] VOL parameter is missing.")
        return

    file_path = os.path.join(VOLUME_ROOT, vol, file_lib, file_name)

    if not os.path.isfile(file_path):
        print(f"[ERROR] File '{file_name}'does not exist in volume '{vol}'in library '{file_lib}'.")
        return

    size = os.path.getsize(file_path)
    modified = datetime.fromtimestamp(os.path.getmtime(file_path)).strftime('%Y-%m-%d %H:%M:%S')
    created = datetime.fromtimestamp(os.path.getctime(file_path)).strftime('%Y-%m-%d %H:%M:%S')
    is_empty = size == 0

    print(f"[INFO] File definition information:")
    print(f"  [PATH] File path        : {file_path}")
    print(f"  [NAME] File name        : {file_name}")
    print(f"  [SIZE] File size        : {size} Byte")
    print(f"  [CREATE] Created date   : {created}")
    print(f"  [MODIFY] Last modified  : {modified}")
    print(f"  [EMPTY] File is empty   : {'Yes' if is_empty else 'No'}")


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
    log_path = os.path.join(VOLUME_ROOT, "JOBLOG", "job.log")
    if not os.path.isfile(log_path):
        print("[INFO] No job history available.")
        return

    print("[INFO] Job history (latest first):")
    with open(log_path, "r") as f:
        lines = f.readlines()
        for line in reversed(lines[-10:]):  # 최근 10개만 표시
            job_id, lib, prog, start, end, status = line.strip().split(',')
            print(f"  [JOB] Job ID: {job_id}")
            print(f"    |- program: {lib}/{prog}")
            print(f"    |- start time: {start}")
            print(f"    |- end time: {end}")
            print(f"    -- status: {status}")
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
    # Yes: CALL PGM-HELLO,VOL-DISK01
    params = dict(item.split('-') for item in command.replace('CALL ', '').split(','))
    pgm = params.get('PGM')
    vol = params.get('VOL')

    if not pgm or not vol:
        print("[ERROR] PGM or VOL parameter is missing.")
        return

    # Construct path
    parts = pgm.split('/')
    if len(parts) != 2:
        print("[ERROR] PGM must be in library/program name format.. ex: PGM-SALES/HELLO")
        return

    lib, prog = parts
    prog_path = os.path.join(VOLUME_ROOT, vol, lib, prog)

    if not os.path.isfile(prog_path):
        print(f"[ERROR] Program '{prog}'does not exist in volume '{vol}'in library '{lib}'.")
        return

    # Determine execution method
    if prog_path.endswith('.py'):
        cmd = ['python3', prog_path]
    elif prog_path.endswith('.sh'):
        cmd = ['bash', prog_path]
    else:
        print(f"[ERROR] Non-executable extension: {prog_path}")
        return

    try:
        print(f"[INFO] Program execution started: {prog_path}")
        result = subprocess.run(cmd, check=True, text=True, capture_output=True)
        print("[OUTPUT]")
        print(result.stdout)
    except subprocess.CalledProcessError as e:
        print(f"[ERROR] Failed to execute:\n{e.stderr}")
def DSPFD(command):
    # Example: DSPFD FILE(SALES/REPORT),VOL-DISK01
    main_part, *others = command.replace('DSPFD ', '').split(',')
    file_lib, file_name = main_part.replace('FILE(', '').replace(')', '').split('/')
    params = dict(item.split('-') for item in others if '-' in item)
    vol = params.get('VOL')

    if not vol:
        print("[ERROR] VOL parameter is missing.")
        return

    file_path = os.path.join(VOLUME_ROOT, vol, file_lib, file_name)

    if not os.path.isfile(file_path):
        print(f"[ERROR] File '{file_name}'does not exist in volume '{vol}'in library '{file_lib}'.")
        return

    size = os.path.getsize(file_path)
    modified = datetime.fromtimestamp(os.path.getmtime(file_path)).strftime('%Y-%m-%d %H:%M:%S')
    created = datetime.fromtimestamp(os.path.getctime(file_path)).strftime('%Y-%m-%d %H:%M:%S')
    is_empty = size == 0

    print(f"[INFO] File definition information:")
    print(f"  [PATH] File path        : {file_path}")
    print(f"  [NAME] File name        : {file_name}")
    print(f"  [SIZE] File size        : {size} Byte")
    print(f"  [CREATE] Created date   : {created}")
    print(f"  [MODIFY] Last modified  : {modified}")
    print(f"  [EMPTY] File is empty   : {'Yes' if is_empty else 'No'}")

def DLTFILE(command):
    # Example: DLTFILE FILE(ACCTLIB/CUSTMAST),VOL-DISK01
    main_part, *others = command.replace('DLTFILE ', '').split(',')
    file_lib, file_name = main_part.replace('FILE(', '').replace(')', '').split('/')
    params = dict(item.split('-') for item in others if '-' in item)
    vol = params.get('VOL')

    if not vol:
        print("[ERROR] VOL parameter is missing.")
        return
def DLTLIB(command):
    # Example: DLTLIB LIB-ACCTLIB,VOL-DISK01
    params = dict(item.split('-') for item in command.replace('DLTLIB ', '').split(','))
    lib = params.get('LIB')
    vol = params.get('VOL')

    if not lib or not vol:
        print("[ERROR] LIB or VOL parameter is missing.")
        return

    path = os.path.join(VOLUME_ROOT, vol, lib)

    if not os.path.exists(path):
        print(f"[ERROR] Library '{lib}'does not exist in volume '{vol}'.")
        return

    try:
        shutil.rmtree(path)
        print(f"[INFO] Library '{lib}'in volume '{vol}'has been deleted.")
    except Exception as e:
        print(f"[ERROR] 삭제 failed: {e}")

def CRTLIB(command):
    # Example: CRTLIB LIB-ACCTLIB,VOL-DISK01
    params = dict(item.split('-') for item in command.replace('CRTLIB ', '').split(','))
    lib = params.get('LIB')
    vol = params.get('VOL')

    if not lib or not vol:
        print("[ERROR] LIB or VOL parameter is missing.")
        return

    path = os.path.join(VOLUME_ROOT, vol, lib)
    os.makedirs(path, exist_ok=True)
    print(f"[INFO] Library '{lib}'in volume '{vol}'has been created: {path}")

def CRTFILE(command):
    # Example: CRTFILE FILE(ACCTLIB/CUSTMAST),VOL-DISK01,ENT-100
    main_part, *others = command.replace('CRTFILE ', '').split(',')
    file_lib, file_name = main_part.replace('FILE(', '').replace(')', '').split('/')
    params = dict(item.split('-') for item in others if '-' in item)
    vol = params.get('VOL')

    if not vol:
        print("[ERROR] VOL parameter is missing.")
        return

    lib_path = os.path.join(VOLUME_ROOT, vol, file_lib)
    file_path = os.path.join(lib_path, file_name)

    if not os.path.exists(lib_path):
        print(f"[ERROR] Library '{file_lib}'does not exist. Please run CRTLIB command first.")
        return

    with open(file_path, 'w') as f:
        f.write("")  # Create empty dataset

    # Update file information in catalog.json
    reclen = params.get('RECLEN', 80)
    rectype = params.get('RECTYPE', 'FB')
    try:
        reclen = int(reclen)
    except:
        reclen = 80
    
    update_catalog_info(vol, file_name, rectype=rectype, reclen=reclen)
    
    print(f"[INFO] '{file_name}'in library '{file_lib}'has been created: {file_path}")
    print(f"[INFO] File information registered in catalog.json (RECTYPE={rectype}, RECLEN={reclen})")
    
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
    Fujitsu ASP EDTFILE command implementation
    Format: EDTFILE FILE(LIB/FILENAME),VOL-VOLUME[,MODE=DISPLAY|EDIT]
    """
    main_part, *others = command.replace('EDTFILE ', '').split(',')
    
    # Parse FILE(LIB/FILENAME) parameter
    if not main_part.startswith('FILE(') or not main_part.endswith(')'):
        print("[ERROR] parameter check faile ex: FILE(LIB/FILENAME)")
        return
    
    file_spec = main_part[5:-1]  # Remove FILE( and )
    if '/' not in file_spec:
        print("[ERROR] File does not exist ex: LIB/FILENAME")
        return
    
    lib, filename = file_spec.split('/', 1)
    
    # Parse additional parameters
    params = {}
    for param in others:
        if '=' in param:
            key, value = param.split('=', 1)
            params[key.strip()] = value.strip()
        elif '-' in param:
            key, value = param.split('-', 1)
            params[key.strip()] = value.strip()
    
    volume = params.get('VOL')
    mode = params.get('MODE', 'DISPLAY').upper()
    
    if not volume:
        print("[ERROR] VOL parameter is missing.")
        return
    
    if mode not in ['DISPLAY', 'EDIT']:
        print("[ERROR] MODE only supports DISPLAY or EDIT.")
        return
    
    # Construct file path
    file_path = os.path.join(VOLUME_ROOT, volume, lib, filename)
    
    if not os.path.isfile(file_path):
        print(f"[ERROR] File '{filename}'does not exist in volume '{volume}'in library '{lib}'.")
        return
    
    # Get file format info from catalog
    file_info = get_file_info(volume, filename)
    reclen = file_info.get('RECLEN', 80)
    rectype = file_info.get('RECTYPE', 'FB')
    encoding = file_info.get('ENCODING', 'utf-8')
    
    print(f"[INFO] EDTFILE - File: {lib}/{filename}, Volume: {volume}, Mode: {mode}")
    print(f"[INFO] Record length: {reclen}, Record type: {rectype}")
    
    # Launch the editor
    if mode == 'DISPLAY':
        _edtfile_display_mode(file_path, reclen, rectype, encoding)
    elif mode == 'EDIT':
        _edtfile_edit_mode(file_path, reclen, rectype, encoding)

def _edtfile_display_mode(file_path, reclen, rectype, encoding='utf-8'):
    """Display mode for EDTFILE - view file contents with curses browser"""
    try:
        with open(file_path, 'rb') as f:
            file_content = f.read()
        
        # Parse records based on record type
        records = []
        raw_records = []  # Store raw binary data for hex display
        pos = 0
        record_num = 1
        
        while pos < len(file_content):
            if rectype == 'FB':  # Fixed Block
                record_data = file_content[pos:pos+reclen]
                if len(record_data) == 0:
                    break
                # Store raw data for hex display
                raw_records.append(record_data)
                
                # For Python terminal, preserve SJIS bytes as-is (no Unicode conversion)
                # Use latin-1 to preserve byte values exactly
                record_str = record_data.decode('latin-1')
                records.append((record_num, record_str))
                pos += reclen
                record_num += 1
            elif rectype == 'VB':  # Variable Block with RDW
                if pos + 4 > len(file_content):
                    break
                # Read RDW (Record Descriptor Word)
                rdw = file_content[pos:pos+4]
                rec_length = int.from_bytes(rdw[:2], 'big')
                if rec_length < 4 or pos + rec_length > len(file_content):
                    break
                
                record_data = file_content[pos:pos+rec_length]
                data_part = record_data[4:]  # Skip RDW
                raw_records.append(record_data)
                
                # For Python terminal, preserve SJIS bytes as-is (no Unicode conversion)
                record_str = data_part.decode('latin-1')
                
                records.append((record_num, record_str))
                pos += rec_length
                record_num += 1
            else:  # LB (Line Block)
                # Variable length records - find line breaks
                end_pos = file_content.find(b'\n', pos)
                if end_pos == -1:
                    record_data = file_content[pos:]
                    pos = len(file_content)
                else:
                    record_data = file_content[pos:end_pos]
                    pos = end_pos + 1
                
                if len(record_data) == 0:
                    break
                
                raw_records.append(record_data)
                # For Python terminal, preserve SJIS bytes as-is (no Unicode conversion)
                record_str = record_data.decode('latin-1')
                records.append((record_num, record_str))
                record_num += 1
        
        if not records:
            print("[INFO] File is empty.")
            return
        
        # Launch curses-based record browser
        _launch_curses_browser(file_path, records, raw_records, rectype, reclen, encoding)
        
    except Exception as e:
        import traceback
        print(f"[ERROR] File processing error: {e}")
        print(f"[DEBUG] Traceback: {traceback.format_exc()}")
        return

def _edtfile_edit_mode(file_path, reclen, rectype, encoding='utf-8'):
    """Edit mode for EDTFILE - interactive editing"""
    print(f"[INFO] Edit mode is currently under development.")
    print(f"[INFO] Please use DISPLAY mode.")
    _edtfile_display_mode(file_path, reclen, rectype, encoding)

def _display_records_paged(records, file_path):
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
    help_text = """
================================================================================
                           ASP SYSTEM COMMAND HELP
================================================================================

Available Commands:

LIBRARY MANAGEMENT:
  CRTLIB LIB-<library>,VOL-<volume>
    - Create a new library in specified volume
    - Example: CRTLIB LIB-TESTLIB,VOL-DISK01

  DLTLIB LIB-<library>,VOL-<volume>
    - Delete an existing library
    - Example: DLTLIB LIB-TESTLIB,VOL-DISK01

  WRKLIB LIB-<library>,VOL-<volume>
    - Display library contents and information
    - Example: WRKLIB LIB-TESTLIB,VOL-DISK01

FILE MANAGEMENT:
  CRTFILE FILE(<library>/<filename>),VOL-<volume>[,RECTYPE-<type>][,RECLEN-<length>]
    - Create a new file with specified attributes
    - RECTYPE: FB (Fixed Block), VB (Variable Block), LB (Line Block)
    - Example: CRTFILE FILE(TESTLIB/EMPLOYEE),VOL-DISK01,RECTYPE-FB,RECLEN-80

  DLTFILE FILE(<library>/<filename>),VOL-<volume>
    - Delete an existing file
    - Example: DLTFILE FILE(TESTLIB/EMPLOYEE),VOL-DISK01

  DSPFD FILE(<library>/<filename>),VOL-<volume>
    - Display file description and attributes
    - Example: DSPFD FILE(TESTLIB/EMPLOYEE),VOL-DISK01

  EDTFILE FILE(<library>/<filename>),VOL-<volume>[,MODE-<mode>]
    - Edit file contents using built-in editor
    - MODE: BROWSE (read-only), DISPLAY (web view), EDIT (interactive)
    - Example: EDTFILE FILE(TESTLIB/EMPLOYEE),VOL-DISK01,MODE-BROWSE

SYSTEM INFORMATION:
  WRKVOL [VOL-<volume>]
    - Display volume information and usage
    - Example: WRKVOL or WRKVOL VOL-DISK01

  WRKOBJ TYPE-<type>,VOL-<volume>[,LIB-<library>]
    - Display objects of specified type
    - TYPE: FILE, LIB, PGM
    - Example: WRKOBJ TYPE-FILE,VOL-DISK01,LIB-TESTLIB

  DSPJOB [JOB-<jobname>]
    - Display job information and status
    - Example: DSPJOB or DSPJOB JOB-BATCH001

JOB MANAGEMENT:
  CALL PGM-<program>[,PARM-'<parameters>']
    - Execute a program with optional parameters
    - Example: CALL PGM-PAYROLL,PARM-'MONTHLY UPDATE'

MESSAGING:
  SNDMSG USER-<user>,MSG-'<message>'
    - Send message to specified user
    - Example: SNDMSG USER-ADMIN,MSG-'System maintenance at 10PM'

  RCVMSG [USER-<user>]
    - Receive messages for current or specified user
    - Example: RCVMSG or RCVMSG USER-ADMIN

  WRKMSG [USER-<user>]
    - Work with messages (display message queue)
    - Example: WRKMSG or WRKMSG USER-ADMIN

LIBRARY BACKUP/RESTORE:
  SAVLIB LIB-<library>,VOL-<volume>,SAV-<savefile>
    - Save library to backup file
    - Example: SAVLIB LIB-TESTLIB,VOL-DISK01,SAV-TESTLIB.SAV

  RSTLIB LIB-<library>,VOL-<volume>,SAV-<savefile>
    - Restore library from backup file
    - Example: RSTLIB LIB-TESTLIB,VOL-DISK01,SAV-TESTLIB.SAV

ENCODING CONVERSION:
  CTTFILE INFILE-<input>,OUTFILE-<output>,INENC-<encoding>,OUTENC-<encoding>
    - Convert file encoding (supports SJIS, UTF-8, ASCII, etc.)
    - Example: CTTFILE INFILE-data.sjis,OUTFILE-data.utf8,INENC-shift_jis,OUTENC-utf-8

GENERAL SYNTAX NOTES:
  - Commands are case-insensitive
  - Parameters are separated by commas
  - Parameter format: KEYWORD-VALUE
  - File paths use library/filename format
  - Quotes are required for parameters containing spaces

EXAMPLES:
  CRTLIB LIB-PAYROLL,VOL-DISK01
  CRTFILE FILE(PAYROLL/EMPLOYEE),VOL-DISK01,RECTYPE-FB,RECLEN-120
  EDTFILE FILE(PAYROLL/EMPLOYEE),VOL-DISK01,MODE-BROWSE
  DSPFD FILE(PAYROLL/EMPLOYEE),VOL-DISK01
  WRKVOL VOL-DISK01

For more detailed information about a specific command, refer to the ASP System 
Administrator's Guide or contact your system administrator.

================================================================================
"""
    print(help_text)
