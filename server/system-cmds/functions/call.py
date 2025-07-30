# -*- coding: utf-8 -*-
"""
CALL (Call Program) Command Implementation for OpenASP

Based on Fujitsu ASP CALL command specifications.
Executes programs in various languages with SMED map integration.
"""

import os
import sys
import json
import subprocess
import re
from datetime import datetime
from typing import Dict, List, Tuple, Optional, Any

# Import from parent module
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from asp_commands import VOLUME_ROOT, get_catalog_info, set_pgmec, reset_pgmec

def CALL(command: str) -> bool:
    """
    CALL command - Call Program
    
    Enhanced CALL command following Fujitsu ASP manual format.
    Format: CALL PGM-<program>[.<library>][,PARA-(<parameters>)][,VOL-<volume>]
    
    Args:
        command: Full CALL command string
        
    Returns:
        True if successful, False otherwise
    """
    try:
        reset_pgmec()
        
        # Parse CALL command
        command_str = command.replace('CALL ', '').strip()
        
        # Parse parameters
        program = None
        library = None
        volume = "DISK01"  # Default volume
        parameters = ""
        
        # Split by comma, but handle nested parentheses in PARA parameter
        parts = []
        current_part = ""
        paren_count = 0
        
        for char in command_str:
            if char == '(':
                paren_count += 1
            elif char == ')':
                paren_count -= 1
            elif char == ',' and paren_count == 0:
                parts.append(current_part.strip())
                current_part = ""
                continue
            current_part += char
        
        if current_part.strip():
            parts.append(current_part.strip())
        
        # Process each part
        for part in parts:
            if part.startswith('PGM-'):
                pgm_spec = part[4:]  # Remove 'PGM-'
                if '.' in pgm_spec:
                    program, library = pgm_spec.split('.', 1)
                else:
                    program = pgm_spec
            elif part.startswith('PARA-(') and part.endswith(')'):
                parameters = part[6:-1]  # Remove 'PARA-(' and ')'
            elif part.startswith('VOL-'):
                volume = part[4:]  # Remove 'VOL-'
        
        if not program:
            print("[ERROR] PGM parameter is required.")
            print("[USAGE] CALL PGM-<program>[.<library>][,PARA-(<parameters>)][,VOL-<volume>]")
            set_pgmec(999)
            return False
        
        print(f"[INFO] Calling program: {program}")
        print(f"[INFO] Library: {library if library else 'Auto-detect'}")
        print(f"[INFO] Volume: {volume}")
        print(f"[INFO] Parameters: {parameters if parameters else 'None'}")
        
        # Find program library if not specified
        if not library:
            library = _find_program_library(volume, program)
            if not library:
                print(f"[ERROR] Program '{program}' not found in any library on volume '{volume}'.")
                set_pgmec(999)
                return False
            print(f"[INFO] Program found in library: {library}")
        
        # Get program information from catalog
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
        
        pgm_type = program_info.get('PGMTYPE', 'UNKNOWN').upper()
        print(f"[INFO] Program type: {pgm_type}")
        
        # Execute program based on type
        if pgm_type == 'JAVA':
            return _call_java_program(volume, library, program, program_info, parameters)
        elif pgm_type == 'COBOL':
            return _call_cobol_program(volume, library, program, program_info, parameters)
        elif pgm_type == 'SHELL':
            return _call_shell_program(volume, library, program, program_info, parameters)
        elif pgm_type == 'PYTHON':
            return _call_python_program(volume, library, program, program_info, parameters)
        else:
            print(f"[ERROR] Unsupported program type: {pgm_type}")
            set_pgmec(999)
            return False
        
    except Exception as e:
        print(f"[ERROR] CALL command failed: {e}")
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

def _call_java_program(volume: str, library: str, program: str, 
                      program_info: Dict[str, Any], parameters: str) -> bool:
    """Execute Java program with SMED map integration"""
    try:
        program_path = os.path.join(VOLUME_ROOT, volume, library)
        
        # Determine Java execution method
        jar_file = program_info.get('JARFILE')
        class_file = program_info.get('CLASSFILE')
        pgm_name = program_info.get('PGMNAME', program)
        
        if jar_file:
            # Execute JAR file
            jar_path = os.path.join(program_path, jar_file)
            if not os.path.exists(jar_path):
                print(f"[ERROR] JAR file not found: {jar_path}")
                set_pgmec(999)
                return False
            
            cmd = ['java', '-jar', jar_path]
        elif class_file:
            # Execute class file
            class_path = os.path.join(program_path, class_file)
            if not os.path.exists(class_path):
                print(f"[ERROR] Class file not found: {class_path}")
                set_pgmec(999)
                return False
            
            # Extract class name from path
            class_name = class_file.replace('/', '.').replace('.class', '')
            cmd = ['java', '-cp', program_path, class_name]
        else:
            # Use program name directly
            cmd = ['java', '-cp', program_path, pgm_name]
        
        # Add parameters if provided
        if parameters:
            param_list = _parse_parameters(parameters)
            cmd.extend(param_list)
        
        print(f"[INFO] Executing Java command: {' '.join(cmd)}")
        
        # Execute the Java program
        try:
            result = subprocess.run(cmd, capture_output=True, text=True, 
                                  cwd=program_path, timeout=30)
            
            print(f"[INFO] Java program executed")
            print(f"[INFO] Return code: {result.returncode}")
            
            if result.stdout:
                print(f"[OUTPUT] {result.stdout}")
                # Process output for SMED integration
                _process_java_output(result.stdout, volume, library, program)
            
            if result.stderr:
                print(f"[ERROR] {result.stderr}")
            
            if result.returncode != 0:
                set_pgmec(result.returncode)
                return False
            
            return True
            
        except subprocess.TimeoutExpired:
            print("[ERROR] Java program execution timed out")
            set_pgmec(999)
            return False
        except Exception as e:
            print(f"[ERROR] Java execution failed: {e}")
            set_pgmec(999)
            return False
        
    except Exception as e:
        print(f"[ERROR] Java program call failed: {e}")
        set_pgmec(999)
        return False

def _call_cobol_program(volume: str, library: str, program: str, 
                       program_info: Dict[str, Any], parameters: str) -> bool:
    """Execute COBOL program (placeholder)"""
    print("[INFO] COBOL program execution not yet implemented")
    print(f"[INFO] Program: {program}")
    print(f"[INFO] Parameters: {parameters}")
    return True

def _call_shell_program(volume: str, library: str, program: str, 
                       program_info: Dict[str, Any], parameters: str) -> bool:
    """Execute Shell script program"""
    try:
        program_path = os.path.join(VOLUME_ROOT, volume, library)
        shell_file = program_info.get('SHELLFILE', f"{program}.sh")
        shell_path = os.path.join(program_path, shell_file)
        
        if not os.path.exists(shell_path):
            print(f"[ERROR] Shell file not found: {shell_path}")
            set_pgmec(999)
            return False
        
        # Make shell file executable
        os.chmod(shell_path, 0o755)
        
        # Prepare command
        cmd = ['bash', shell_path]
        
        # Add parameters
        if parameters:
            param_list = _parse_parameters(parameters)
            cmd.extend(param_list)
        
        print(f"[INFO] Executing shell command: {' '.join(cmd)}")
        
        # Set environment variables
        env = os.environ.copy()
        env['ASP_VOLUME'] = volume
        env['ASP_LIBRARY'] = library
        env['ASP_PROGRAM'] = program
        
        try:
            result = subprocess.run(cmd, capture_output=True, text=True, 
                                  cwd=program_path, env=env, timeout=30)
            
            print(f"[INFO] Shell program executed")
            print(f"[INFO] Return code: {result.returncode}")
            
            if result.stdout:
                print(f"[OUTPUT] {result.stdout}")
            
            if result.stderr:
                print(f"[ERROR] {result.stderr}")
            
            if result.returncode != 0:
                set_pgmec(result.returncode)
                return False
            
            return True
            
        except subprocess.TimeoutExpired:
            print("[ERROR] Shell program execution timed out")
            set_pgmec(999)
            return False
        except Exception as e:
            print(f"[ERROR] Shell execution failed: {e}")
            set_pgmec(999)
            return False
        
    except Exception as e:
        print(f"[ERROR] Shell program call failed: {e}")
        set_pgmec(999)
        return False

def _call_python_program(volume: str, library: str, program: str, 
                        program_info: Dict[str, Any], parameters: str) -> bool:
    """Execute Python script program"""
    try:
        program_path = os.path.join(VOLUME_ROOT, volume, library)
        python_file = program_info.get('PYTHONFILE', f"{program}.py")
        python_path = os.path.join(program_path, python_file)
        
        if not os.path.exists(python_path):
            print(f"[ERROR] Python file not found: {python_path}")
            set_pgmec(999)
            return False
        
        # Prepare command
        cmd = ['python3', python_path]
        
        # Add parameters
        if parameters:
            param_list = _parse_parameters(parameters)
            cmd.extend(param_list)
        
        print(f"[INFO] Executing Python command: {' '.join(cmd)}")
        
        try:
            result = subprocess.run(cmd, capture_output=True, text=True, 
                                  cwd=program_path, timeout=30)
            
            print(f"[INFO] Python program executed")
            print(f"[INFO] Return code: {result.returncode}")
            
            if result.stdout:
                print(f"[OUTPUT] {result.stdout}")
            
            if result.stderr:
                print(f"[ERROR] {result.stderr}")
            
            if result.returncode != 0:
                set_pgmec(result.returncode)
                return False
            
            return True
            
        except subprocess.TimeoutExpired:
            print("[ERROR] Python program execution timed out")
            set_pgmec(999)
            return False
        except Exception as e:
            print(f"[ERROR] Python execution failed: {e}")
            set_pgmec(999)
            return False
        
    except Exception as e:
        print(f"[ERROR] Python program call failed: {e}")
        set_pgmec(999)
        return False

def _parse_parameters(param_string: str) -> List[str]:
    """Parse parameter string into list of arguments"""
    try:
        # Simple parameter parsing - can be enhanced as needed
        if not param_string:
            return []
        
        # Handle comma-separated parameters
        params = []
        for param in param_string.split(','):
            param = param.strip()
            if param:
                params.append(param)
        
        return params
        
    except Exception as e:
        print(f"[DEBUG] Parameter parsing error: {e}")
        return []

def _process_java_output(output: str, volume: str, library: str, program: str) -> None:
    """Process Java program output for SMED integration (simplified)"""
    try:
        # Look for JSON output for SMED integration
        lines = output.strip().split('\n')
        for line in lines:
            if line.strip().startswith('{') and line.strip().endswith('}'):
                try:
                    output_data = json.loads(line.strip())
                    action = output_data.get('action')
                    
                    if action == 'display_map':
                        map_name = output_data.get('map_name')
                        print(f"[INFO] SMED map display requested: {map_name}")
                        # Simplified SMED integration
                        print(f"[INFO] Map data: {output_data.get('data', {})}")
                    elif action == 'continue':
                        print(f"[INFO] Program continuation requested")
                    
                except json.JSONDecodeError:
                    pass  # Not JSON, ignore
                    
    except Exception as e:
        print(f"[DEBUG] Output processing error: {e}")


# For backwards compatibility and testing
if __name__ == "__main__":
    import sys
    if len(sys.argv) > 1:
        CALL(' '.join(sys.argv[1:]))