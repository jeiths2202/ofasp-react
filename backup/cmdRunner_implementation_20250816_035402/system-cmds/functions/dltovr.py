#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
DLTOVR (Delete Override) Command Implementation with dslock_suite Integration

Handles delete file override commands that integrate with dslock_suite for
distributed file locking and override mapping management.

Usage:
    DLTOVR FILE(logical-name)
    
Example:
    DLTOVR FILE(EMP-FILE)
"""

import os
import sys
import json
import subprocess
import threading
from typing import Dict, Optional, Tuple
from datetime import datetime

# Import override mappings from ovrf module
try:
    from .ovrf import override_mappings, override_locks, mapping_lock, call_dslock_release
    OVRF_AVAILABLE = True
except ImportError:
    # Fallback for standalone execution
    override_mappings = {}
    override_locks = {}
    mapping_lock = threading.Lock()
    OVRF_AVAILABLE = False

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

def call_dslock_release_local(dataset: str) -> Tuple[bool, str]:
    """
    Local implementation of dslock_release call
    
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

def parse_dltovr_command(command_line: str) -> Dict[str, str]:
    """
    Parse DLTOVR command line
    
    Args:
        command_line: Command line string
        
    Returns:
        Dictionary of parsed parameters
    """
    params = {}
    
    # Remove command name
    parts = command_line.replace("DLTOVR", "").strip().split()
    
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

def DLTOVR(command_line: str) -> bool:
    """
    Execute DLTOVR (Delete Override) command with dslock_suite integration
    
    Args:
        command_line: Full command line
        
    Returns:
        True if successful, False otherwise
    """
    try:
        print(f"[DLTOVR] Processing: {command_line}")
        
        # Parse command parameters
        params = parse_dltovr_command(command_line)
        
        # Validate required parameters
        if "FILE" not in params:
            print("[ERROR] FILE parameter is required")
            return False
        
        logical_name = params["FILE"]
        print(f"[DLTOVR] Logical file: {logical_name}")
        
        # Check if override mapping exists
        with mapping_lock:
            if logical_name not in override_mappings:
                print(f"[ERROR] No override mapping found for: {logical_name}")
                print(f"[INFO] Available mappings: {list(override_mappings.keys())}")
                return False
            
            # Get mapping info
            mapping_info = override_mappings[logical_name]
            dataset_name = mapping_info.get("dataset_name")
            physical_file = mapping_info.get("physical_file")
            
            print(f"[DLTOVR] Found mapping: {logical_name} -> {physical_file}")
            print(f"[DLTOVR] Dataset name: {dataset_name}")
        
        # Release lock using dslock_suite
        if dataset_name:
            print(f"[DLTOVR] Releasing dslock for dataset: {dataset_name}")
            
            # Use local implementation or imported function
            if OVRF_AVAILABLE:
                success, error_msg = call_dslock_release(dataset_name)
            else:
                success, error_msg = call_dslock_release_local(dataset_name)
            
            if not success:
                print(f"[WARN] Failed to release dslock: {error_msg}")
                # Continue with mapping removal even if lock release fails
            else:
                print(f"[DLTOVR] Successfully released dslock for: {dataset_name}")
        
        # Remove from override mapping table
        with mapping_lock:
            if logical_name in override_mappings:
                removed_mapping = override_mappings.pop(logical_name)
                print(f"[DLTOVR] Removed mapping: {logical_name}")
                print(f"[DLTOVR] Mapping details: {removed_mapping}")
            
            if logical_name in override_locks:
                override_locks.pop(logical_name)
                print(f"[DLTOVR] Removed lock reference: {logical_name}")
        
        print(f"[DLTOVR] Override deletion completed for: {logical_name}")
        print(f"[DLTOVR] Remaining mappings: {len(override_mappings)}")
        
        return True
        
    except Exception as e:
        print(f"[ERROR] Exception in DLTOVR: {str(e)}")
        return False

def cleanup_all_overrides() -> int:
    """
    Cleanup all current overrides and release locks
    
    Returns:
        Number of overrides cleaned up
    """
    cleanup_count = 0
    
    try:
        with mapping_lock:
            logical_names = list(override_mappings.keys())
        
        for logical_name in logical_names:
            print(f"[CLEANUP] Processing override: {logical_name}")
            command = f"DLTOVR FILE({logical_name})"
            if DLTOVR(command):
                cleanup_count += 1
            
        print(f"[CLEANUP] Cleaned up {cleanup_count} overrides")
        return cleanup_count
        
    except Exception as e:
        print(f"[ERROR] Exception during cleanup: {str(e)}")
        return cleanup_count

def list_active_overrides() -> Dict:
    """List all active override mappings"""
    with mapping_lock:
        return override_mappings.copy()

def get_override_info(logical_name: str) -> Optional[Dict]:
    """Get information about a specific override mapping"""
    with mapping_lock:
        return override_mappings.get(logical_name)

# Test function
if __name__ == "__main__":
    # Test DLTOVR command
    test_command = "DLTOVR FILE(EMP-FILE)"
    result = DLTOVR(test_command)
    print(f"Test result: {result}")
    
    # Show remaining mappings
    print("Remaining mappings:", list_active_overrides())