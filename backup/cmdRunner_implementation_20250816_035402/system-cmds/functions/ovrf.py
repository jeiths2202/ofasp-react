#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
OVRF (Override File) Command Implementation with dslock_suite Integration

Handles file override commands that integrate with dslock_suite for
distributed file locking and override mapping management.

Usage:
    OVRF FILE(logical-name) TOFILE(physical-file) TYPE(*DATA)
    
Example:
    OVRF FILE(EMP-FILE) TOFILE(EMPLOYEE.FB.TESTLIB) TYPE(*DATA)
"""

import os
import sys
import json
import subprocess
import threading
from typing import Dict, Optional, Tuple
from datetime import datetime

# Global override mapping table
override_mappings = {}
override_locks = {}
mapping_lock = threading.Lock()

# Configuration
DSLOCK_SUITE_PATH = "/home/aspuser/app/ofasp-refactor/dslock_suite"
DSLOCK_CONFIG = {
    "DSIO_ROOT": "/dev/shm",
    "DSIO_ATOMIC": "1",
    "RECFM_FB_NEWLINE": "1",
    "RECFM_VB_NEWLINE": "0"
}

def setup_dslock_environment():
    """Setup environment variables for dslock_suite"""
    for key, value in DSLOCK_CONFIG.items():
        os.environ[key] = value
    
    # Set config path if not already set
    if "DSIO_CONFIG" not in os.environ:
        config_path = os.path.join(DSLOCK_SUITE_PATH, "config", "config.json")
        if os.path.exists(config_path):
            os.environ["DSIO_CONFIG"] = config_path

def call_dslock_acquire(dataset: str, level: str = "MOD") -> Tuple[bool, str]:
    """
    Call dslock_acquire to acquire a lock on a dataset
    
    Args:
        dataset: Dataset name to lock
        level: Lock level (SHR, OLD, MOD)
        
    Returns:
        Tuple of (success, error_message)
    """
    try:
        # Build test program that calls dslock_acquire
        test_program = f"""
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "{DSLOCK_SUITE_PATH}/src/include/dslock.h"

int main() {{
    char errbuf[256];
    int result = dslock_acquire("{dataset}", "{level}", errbuf, sizeof(errbuf));
    
    if (result == 0) {{
        printf("SUCCESS");
        return 0;
    }} else {{
        printf("ERROR: %s (code: %d)", errbuf, result);
        return result;
    }}
}}
"""
        
        # Create temporary C file
        temp_dir = "/tmp"
        temp_c_file = os.path.join(temp_dir, f"dslock_test_{os.getpid()}.c")
        temp_exe_file = os.path.join(temp_dir, f"dslock_test_{os.getpid()}")
        
        try:
            # Write C source
            with open(temp_c_file, 'w') as f:
                f.write(test_program)
            
            # Setup environment
            setup_dslock_environment()
            
            # Compile
            compile_cmd = [
                "gcc",
                "-I" + os.path.join(DSLOCK_SUITE_PATH, "src"),
                "-L" + os.path.join(DSLOCK_SUITE_PATH, "build"),
                "-o", temp_exe_file,
                temp_c_file,
                "-ldslock"
            ]
            
            result = subprocess.run(compile_cmd, capture_output=True, text=True)
            if result.returncode != 0:
                return False, f"Compilation failed: {result.stderr}"
            
            # Execute
            result = subprocess.run([temp_exe_file], capture_output=True, text=True)
            
            if result.stdout.startswith("SUCCESS"):
                return True, ""
            else:
                return False, result.stdout
                
        finally:
            # Cleanup temporary files
            for temp_file in [temp_c_file, temp_exe_file]:
                if os.path.exists(temp_file):
                    try:
                        os.remove(temp_file)
                    except:
                        pass
                        
    except Exception as e:
        return False, f"Exception during dslock_acquire: {str(e)}"

def call_dslock_release(dataset: str) -> Tuple[bool, str]:
    """
    Call dslock_release to release a lock on a dataset
    
    Args:
        dataset: Dataset name to release
        
    Returns:
        Tuple of (success, error_message)
    """
    try:
        # Build test program that calls dslock_release
        test_program = f"""
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "{DSLOCK_SUITE_PATH}/src/include/dslock.h"

int main() {{
    char errbuf[256];
    int result = dslock_release("{dataset}", errbuf, sizeof(errbuf));
    
    if (result == 0) {{
        printf("SUCCESS");
        return 0;
    }} else {{
        printf("ERROR: %s (code: %d)", errbuf, result);
        return result;
    }}
}}
"""
        
        # Create temporary C file
        temp_dir = "/tmp"
        temp_c_file = os.path.join(temp_dir, f"dslock_release_{os.getpid()}.c")
        temp_exe_file = os.path.join(temp_dir, f"dslock_release_{os.getpid()}")
        
        try:
            # Write C source
            with open(temp_c_file, 'w') as f:
                f.write(test_program)
            
            # Setup environment
            setup_dslock_environment()
            
            # Compile
            compile_cmd = [
                "gcc",
                "-I" + os.path.join(DSLOCK_SUITE_PATH, "src"),
                "-L" + os.path.join(DSLOCK_SUITE_PATH, "build"),
                "-o", temp_exe_file,
                temp_c_file,
                "-ldslock"
            ]
            
            result = subprocess.run(compile_cmd, capture_output=True, text=True)
            if result.returncode != 0:
                return False, f"Compilation failed: {result.stderr}"
            
            # Execute
            result = subprocess.run([temp_exe_file], capture_output=True, text=True)
            
            if result.stdout.startswith("SUCCESS"):
                return True, ""
            else:
                return False, result.stdout
                
        finally:
            # Cleanup temporary files
            for temp_file in [temp_c_file, temp_exe_file]:
                if os.path.exists(temp_file):
                    try:
                        os.remove(temp_file)
                    except:
                        pass
                        
    except Exception as e:
        return False, f"Exception during dslock_release: {str(e)}"

def parse_ovrf_command(command_line: str) -> Dict[str, str]:
    """
    Parse OVRF command line
    
    Args:
        command_line: Command line string
        
    Returns:
        Dictionary of parsed parameters
    """
    params = {}
    
    # Remove command name
    parts = command_line.replace("OVRF", "").strip().split()
    
    for part in parts:
        if "(" in part and ")" in part:
            # Extract parameter name and value
            param_name = part.split("(")[0]
            param_value = part.split("(")[1].rstrip(")")
            params[param_name] = param_value
        else:
            # Boolean parameter or keyword
            params[part] = None
    
    return params

def resolve_physical_filename(tofile: str) -> Tuple[str, Optional[str]]:
    """
    Resolve TOFILE parameter to physical filename and validate against catalog
    
    Args:
        tofile: TOFILE parameter value (e.g., EMPLOYEE.FB.TESTLIB)
        
    Returns:
        Tuple of (physical_file_path, dataset_key) or (tofile, None) if not found
    """
    # Parse format: dataset.library (e.g., EMPLOYEE.FB.TESTLIB)
    parts = tofile.split(".")
    
    if len(parts) >= 2:
        # Extract library (last part) and dataset (everything before)
        library = parts[-1]
        dataset = ".".join(parts[:-1])
        
        print(f"[OVRF] Parsing: library={library}, dataset={dataset}")
        
        # Check catalog for this dataset in the library
        catalog_path = "/home/aspuser/app/config/catalog.json"
        try:
            with open(catalog_path, 'r', encoding='utf-8') as f:
                catalog = json.load(f)
            
            # Look for dataset in DISK01.library
            if "DISK01" in catalog and library in catalog["DISK01"]:
                if dataset in catalog["DISK01"][library]:
                    # Found in catalog, construct physical path
                    volume_path = f"/home/aspuser/app/volume/DISK01/{library}"
                    physical_file = os.path.join(volume_path, dataset)
                    dataset_key = f"DISK01.{library}.{dataset}"
                    
                    print(f"[OVRF] Found in catalog: {dataset_key}")
                    print(f"[OVRF] Physical path: {physical_file}")
                    
                    return physical_file, dataset_key
                else:
                    print(f"[OVRF] Dataset {dataset} not found in library {library}")
            else:
                print(f"[OVRF] Library {library} not found in DISK01")
                
        except Exception as e:
            print(f"[ERROR] Failed to read catalog: {str(e)}")
    
    # Fallback to direct interpretation
    print(f"[OVRF] Using direct interpretation: {tofile}")
    return tofile, None

def OVRF(command_line: str) -> bool:
    """
    Execute OVRF (Override File) command with dslock_suite integration
    
    Args:
        command_line: Full command line
        
    Returns:
        True if successful, False otherwise
    """
    try:
        print(f"[OVRF] Processing: {command_line}")
        
        # Parse command parameters
        params = parse_ovrf_command(command_line)
        
        # Validate required parameters
        if "FILE" not in params:
            print("[ERROR] FILE parameter is required")
            return False
            
        if "TOFILE" not in params:
            print("[ERROR] TOFILE parameter is required")
            return False
        
        logical_name = params["FILE"]
        physical_file = params["TOFILE"]
        override_type = params.get("TYPE", "*DATA")
        
        print(f"[OVRF] Logical: {logical_name}, Physical: {physical_file}, Type: {override_type}")
        
        # Resolve physical filename and get dataset key
        resolved_physical, dataset_key = resolve_physical_filename(physical_file)
        print(f"[OVRF] Resolved physical path: {resolved_physical}")
        
        # Check if physical file exists
        if not os.path.exists(resolved_physical):
            print(f"[ERROR] Physical file does not exist: {resolved_physical}")
            return False
        
        # Use dataset key for dslock if available, otherwise fallback to original name
        if dataset_key:
            dataset_name = dataset_key.replace(".", "_")  # Convert to valid dataset name
        else:
            dataset_name = physical_file.replace(".", "_")  # Convert to valid dataset name
        
        print(f"[OVRF] Acquiring dslock for dataset: {dataset_name}")
        
        success, error_msg = call_dslock_acquire(dataset_name, "MOD")
        
        if not success:
            print(f"[ERROR] Failed to acquire dslock: {error_msg}")
            return False
        
        print(f"[OVRF] Successfully acquired dslock for: {dataset_name}")
        
        # Add to override mapping table
        with mapping_lock:
            override_mappings[logical_name] = {
                "physical_file": physical_file,
                "resolved_path": resolved_physical,
                "dataset_key": dataset_key,
                "dataset_name": dataset_name,
                "type": override_type,
                "created": datetime.now().isoformat(),
                "pid": os.getpid()
            }
            
            override_locks[logical_name] = dataset_name
        
        print(f"[OVRF] Override mapping created: {logical_name} -> {physical_file}")
        print(f"[OVRF] Current mappings: {len(override_mappings)}")
        
        return True
        
    except Exception as e:
        print(f"[ERROR] Exception in OVRF: {str(e)}")
        return False

def get_override_mappings() -> Dict:
    """Get current override mappings"""
    with mapping_lock:
        return override_mappings.copy()

def get_override_locks() -> Dict:
    """Get current override locks"""
    with mapping_lock:
        return override_locks.copy()

# Test function
if __name__ == "__main__":
    # Test OVRF command
    test_command = "OVRF FILE(EMP-FILE) TOFILE(EMPLOYEE.FB.TESTLIB) TYPE(*DATA)"
    result = OVRF(test_command)
    print(f"Test result: {result}")
    
    # Show mappings
    print("Current mappings:", get_override_mappings())