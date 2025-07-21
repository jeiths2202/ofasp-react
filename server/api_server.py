#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
OpenASP API Server v0.5.1
Enhanced with Multi-Type Program Support (JAVA, COBOL, SHELL)
Fixed configuration loading and error handling
"""

import os
import sys
import json
import subprocess
import threading
import time
import ctypes
import psutil
import shutil
from pathlib import Path
from flask import Flask, request, jsonify, send_from_directory
from flask_cors import CORS
import logging
from datetime import datetime
from collections import deque
import uuid
from io import StringIO

# ?? ??
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s [%(levelname)s] %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger(__name__)

app = Flask(__name__)
CORS(app, origins=['http://localhost:3005', 'http://localhost:3000', 'http://localhost:3007', 'http://localhost:3006'])

# ?? ??
VOLUME_ROOT = "/home/aspuser/app/volume"
SMED_DIR = None
ACCOUNT_FILE = None
SMED_PGM_FILE = None
MAP_PGM_FILE = None
accounts = {}
smed_pgm_config = {}
map_pgm_config = {}
java_manager = None

# Log storage (keep last 1000 logs)
execution_logs = deque(maxlen=1000)

class MultiTypeExecutor:
    """?? ???? ?? ???"""
    
    def __init__(self, jar_path=None):
        self.jar_path = jar_path
        self.java_available = self._check_java_availability()
        self.cobol_libs = {}  # ??? COBOL ????? ??
        
    def _check_java_availability(self):
        """Java ?? ?? ?? ??"""
        try:
            result = subprocess.run(['java', '-version'], 
                                  capture_output=True, text=True, timeout=5)
            return result.returncode == 0
        except Exception:
            return False
    
    def execute_java_program(self, program_class, input_data):
        """Java ???? ??"""
        if not self.java_available:
            raise Exception("Java is not available")
        
        if not self.jar_path or not os.path.exists(self.jar_path):
            raise Exception(f"JAR file not found: {self.jar_path}")
        
        try:
            # Java ???? ??
            cmd = ['java', '-jar', self.jar_path, program_class]
            
            logger.info(f"Executing Java command: {' '.join(cmd)}")
            
            process = subprocess.Popen(
                cmd,
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True
            )
            
            # ?? ??? ??
            input_json = json.dumps(input_data) if input_data else "{}"
            logger.info(f"Sending input to Java program: {input_json}")
            
            try:
                stdout, stderr = process.communicate(input=input_json, timeout=30)
            except subprocess.TimeoutExpired:
                process.kill()
                stdout, stderr = process.communicate()
                raise Exception("Java program execution timeout")
            
            logger.info(f"Java program stdout: {stdout[:200]}...")
            if stderr:
                logger.warning(f"Java program stderr: {stderr[:200]}...")
            
            if process.returncode != 0:
                raise Exception(f"Java program failed with exit code {process.returncode}: {stderr}")
            
            # JSON ?? ?? ??
            try:
                result = json.loads(stdout.strip())
                logger.info(f"Successfully parsed JSON result from Java program")
                return result
            except json.JSONDecodeError as e:
                logger.warning(f"Failed to parse JSON from Java output: {e}")
                # JSON? ?? ?? ???? ??
                return {"output": stdout.strip(), "type": "text"}
                
        except Exception as e:
            logger.error(f"Java execution error: {str(e)}")
            raise Exception(f"Java execution error: {str(e)}")
    
    def execute_cobol_program(self, lib_path, input_data):
        """COBOL ???? ?? (dlcall)"""
        if not os.path.exists(lib_path):
            raise Exception(f"COBOL library not found: {lib_path}")
        
        try:
            # COBOL ????? ?? (?? ??)
            if lib_path not in self.cobol_libs:
                self.cobol_libs[lib_path] = ctypes.CDLL(lib_path)
            
            cobol_lib = self.cobol_libs[lib_path]
            
            # COBOL ????? ?? ??? ??? ?? ??
            # ???? COBOL ????? main ?? ?????? ??? ??? ??
            program_name = os.path.splitext(os.path.basename(lib_path))[0]
            
            # ???? COBOL ??? ????? ??
            entry_points = ['main', program_name.lower(), program_name.upper()]
            
            for entry_point in entry_points:
                try:
                    func = getattr(cobol_lib, entry_point)
                    
                    # ?? ???? JSON ???? ??
                    input_json = json.dumps(input_data) if input_data else "{}"
                    input_bytes = input_json.encode('utf-8')
                    
                    # COBOL ?? ?? (????? ??? ???? ??)
                    func.argtypes = [ctypes.c_char_p]
                    func.restype = ctypes.c_char_p
                    
                    result = func(input_bytes)
                    
                    if result:
                        result_str = result.decode('utf-8')
                        try:
                            return json.loads(result_str)
                        except json.JSONDecodeError:
                            return {"output": result_str, "type": "text"}
                    else:
                        return {"output": "COBOL program executed successfully", "type": "text"}
                        
                except AttributeError:
                    continue  # ?? ??? ??? ??
            
            raise Exception(f"No valid entry point found in COBOL library: {lib_path}")
            
        except Exception as e:
            raise Exception(f"COBOL execution error: {str(e)}")
    
    def execute_shell_script(self, script_path, input_data):
        """Shell ???? ??"""
        if not os.path.exists(script_path):
            raise Exception(f"Shell script not found: {script_path}")
        
        if not os.access(script_path, os.X_OK):
            raise Exception(f"Shell script is not executable: {script_path}")
        
        try:
            # ????? ?? ??? ??
            env = os.environ.copy()
            if input_data:
                for key, value in input_data.items():
                    env[f"SMED_{key.upper()}"] = str(value)
                env['SMED_INPUT_JSON'] = json.dumps(input_data)
            
            # Shell ???? ??
            process = subprocess.Popen(
                [script_path],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                env=env,
                timeout=30
            )
            
            stdout, stderr = process.communicate()
            
            if process.returncode != 0:
                raise Exception(f"Shell script failed (exit code {process.returncode}): {stderr}")
            
            # JSON ?? ?? ??
            try:
                return json.loads(stdout.strip())
            except json.JSONDecodeError:
                return {"output": stdout.strip(), "type": "text"}
                
        except subprocess.TimeoutExpired:
            process.kill()
            raise Exception("Shell script execution timeout")
        except Exception as e:
            raise Exception(f"Shell execution error: {str(e)}")
    
    def execute_program(self, program_type, program_path, input_data):
        """???? ??? ?? ??"""
        program_type = program_type.upper()
        
        logger.info(f"Executing {program_type} program: {program_path}")
        
        if program_type == "JAVA":
            return self.execute_java_program(program_path, input_data)
        elif program_type == "COBOL":
            return self.execute_cobol_program(program_path, input_data)
        elif program_type == "SHELL":
            return self.execute_shell_script(program_path, input_data)
        else:
            raise Exception(f"Unsupported program type: {program_type}")

# ?? ??? ????
multi_executor = None

def load_config():
    """?? ??? ??"""
    global SMED_DIR, ACCOUNT_FILE, SMED_PGM_FILE, MAP_PGM_FILE
    global accounts, smed_pgm_config, map_pgm_config, multi_executor
    
    # ?? ?? ??
    script_dir = os.path.dirname(os.path.abspath(__file__))
    app_dir = os.path.dirname(script_dir)
    
    SMED_DIR = os.path.join(app_dir, 'public', 'SMED_FILES')
    ACCOUNT_FILE = os.path.join(app_dir, 'src', 'account.json')
    SMED_PGM_FILE = os.path.join(app_dir, 'src', 'smed_pgm.json')
    MAP_PGM_FILE = os.path.join(app_dir, 'src', 'map_pgm.json')
    
    # JAR ?? ??
    jar_path = os.path.join(script_dir, 'java_jars', 'ofasp.jar')
    
    # Multi-Type Executor ???
    multi_executor = MultiTypeExecutor(jar_path)
    
    logger.info(f"SMED directory: {SMED_DIR}")
    logger.info(f"Account file: {ACCOUNT_FILE}")
    logger.info(f"SMED-PGM file: {SMED_PGM_FILE}")
    logger.info(f"MAP-PGM file: {MAP_PGM_FILE}")
    logger.info(f"JAR file: {jar_path}")
    
    # ?? ?? ??
    try:
        if os.path.exists(ACCOUNT_FILE):
            with open(ACCOUNT_FILE, 'r', encoding='utf-8') as f:
                accounts = json.load(f)
            logger.info(f"Loaded {len(accounts)} user accounts")
        else:
            logger.warning(f"Account file not found: {ACCOUNT_FILE}")
            accounts = {}
    except Exception as e:
        logger.error(f"Failed to load accounts: {e}")
        accounts = {}
    
    # SMED-PGM ?? ?? (??? ??)
    try:
        if os.path.exists(SMED_PGM_FILE):
            with open(SMED_PGM_FILE, 'r', encoding='utf-8') as f:
                raw_config = json.load(f)
            
            # プログラム設定が programs キーの下に ある場合の 処理
            if 'programs' in raw_config and isinstance(raw_config['programs'], dict):
                smed_pgm_config = {}
                for key, value in raw_config['programs'].items():
                    if isinstance(value, dict) and 'TYPE' in value and 'PGM' in value:
                        smed_pgm_config[key] = value
                    else:
                        logger.warning(f"Invalid SMED-PGM config for {key}: missing TYPE or PGM")
            else:
                # 旧形式：メタデータキー 以外の 全ての キーを プログラム設定として 扱う
                metadata_keys = ['description', 'version', 'updated', 'last_updated', 'type_settings', 'package_structure']
                smed_pgm_config = {}
                
                for key, value in raw_config.items():
                    if key not in metadata_keys:
                        if isinstance(value, dict) and 'TYPE' in value and 'PGM' in value:
                            smed_pgm_config[key] = value
                        else:
                            logger.warning(f"Invalid SMED-PGM config for {key}: missing TYPE or PGM")
            
            logger.info(f"Loaded SMED-PGM config: {len(smed_pgm_config)} valid maps")
            
            # ?? ?? ?? ??
            for map_name, config in smed_pgm_config.items():
                logger.info(f"  {map_name}: {config['TYPE']} -> {config['PGM']}")
                
        else:
            logger.warning(f"SMED-PGM file not found: {SMED_PGM_FILE}")
            smed_pgm_config = {}
    except Exception as e:
        logger.error(f"Failed to load SMED-PGM config: {e}")
        smed_pgm_config = {}
    
    # MAP-PGM ?? ?? (?? ???)
    try:
        if os.path.exists(MAP_PGM_FILE):
            with open(MAP_PGM_FILE, 'r', encoding='utf-8') as f:
                map_pgm_config = json.load(f)
            logger.info(f"Loaded MAP-PGM config (legacy): {len(map_pgm_config.get('maps', {}))} maps")
        else:
            map_pgm_config = {}
    except Exception as e:
        logger.error(f"Failed to load MAP-PGM config: {e}")
        map_pgm_config = {}

def get_smed_file_path(map_name):
    """SMED ?? ?? ?? (???? ?? ??)"""
    if not SMED_DIR:
        return None
    
    # ?? ???? ??
    file_path = os.path.join(SMED_DIR, map_name)
    if os.path.exists(file_path):
        return file_path
    
    # ???? ???? ??
    upper_path = os.path.join(SMED_DIR, map_name.upper())
    if os.path.exists(upper_path):
        return upper_path
    
    # ???? ???? ??
    lower_path = os.path.join(SMED_DIR, map_name.lower())
    if os.path.exists(lower_path):
        return lower_path
    
    return None

def parse_smed_file(file_path):
    """SMEDファイル解析"""
    try:
        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()
        
        # SMEDファイルの行を解析
        lines = content.split('\n')
        fields = []
        
        for line in lines:
            line = line.strip()
            if line and not line.startswith('#') and 'ITEM' in line:
                # ITEM行を解析
                field_data = {'name': '', 'type': 'input', 'position': {'row': 1, 'col': 1}, 'length': 10, 'prompt': '', 'color': '#00FF00'}
                
                # ITEMの後のフィールド名を取得
                parts = line.split()
                if len(parts) > 1:
                    field_data['name'] = parts[1]
                
                # POS=の解析
                if 'POS=' in line:
                    pos_match = line.split('POS=')[1].split()[0]
                    if pos_match.startswith('(') and ')' in pos_match:
                        pos_str = pos_match.strip('()')
                        if ',' in pos_str:
                            try:
                                row, col = pos_str.split(',')
                                field_data['position']['row'] = int(row.strip())
                                field_data['position']['col'] = int(col.strip())
                            except ValueError:
                                pass
                
                # LEN=の解析
                if 'LEN=' in line:
                    len_match = line.split('LEN=')[1].split()[0]
                    try:
                        field_data['length'] = int(len_match)
                        field_data['type'] = 'input'  # LENがあれば入力フィールド
                    except ValueError:
                        pass
                
                # PROMPT=の解析
                if 'PROMPT=' in line:
                    prompt_start = line.find('PROMPT=') + 7
                    if prompt_start < len(line):
                        prompt_part = line[prompt_start:].strip()
                        if prompt_part.startswith('"'):
                            # クォートで囲まれたpromptを抽出
                            end_quote = prompt_part.find('"', 1)
                            if end_quote > 0:
                                field_data['prompt'] = prompt_part[1:end_quote]
                                field_data['type'] = 'text'  # PROMPTがあれば表示フィールド
                            else:
                                # 終了クォートが見つからない場合、行末まで取得
                                field_data['prompt'] = prompt_part[1:]
                                field_data['type'] = 'text'
                
                # COLOR=の解析
                if 'COLOR=' in line:
                    color_match = line.split('COLOR=')[1].split()[0]
                    field_data['color'] = color_match
                
                # TYPE=の解析
                if 'TYPE=' in line:
                    type_match = line.split('TYPE=')[1].split()[0]
                    if type_match.upper() == 'T':
                        field_data['type'] = 'text'
                
                fields.append(field_data)
        
        return {
            'map_name': os.path.basename(file_path),
            'fields': fields,
            'raw_content': content[:1000]  # 最初の1000文字
        }
    
    except Exception as e:
        logger.error(f"Failed to parse SMED file {file_path}: {e}")
        return None

def execute_map_program(map_name, input_data):
    """?? ??? ???? ??"""
    global smed_pgm_config, multi_executor
    
    map_name_upper = map_name.upper()
    
    # SMED-PGM ???? ???? ?? ??
    if map_name_upper not in smed_pgm_config:
        logger.warning(f"No program configuration found for map: {map_name}")
        add_log('WARNING', 'EXECUTE', f'No program configuration found for map: {map_name}', {'map_name': map_name})
        return None
    
    config = smed_pgm_config[map_name_upper]
    program_type = config.get('TYPE', '').upper()
    program_path = config.get('PGM', '')
    
    if not program_type or not program_path:
        logger.error(f"Invalid program configuration for map {map_name}: {config}")
        add_log('ERROR', 'EXECUTE', f'Invalid program configuration for map: {map_name}', {'map_name': map_name, 'config': config})
        return None
    
    # Extract program name based on type
    program_name = program_path
    if program_type == 'JAVA':
        # Extract Java class name and package
        program_name = program_path
        package_name = '.'.join(program_path.split('.')[:-1]) if '.' in program_path else 'default'
        class_name = program_path.split('.')[-1] if '.' in program_path else program_path
    elif program_type == 'SHELL':
        # Extract shell script name
        program_name = os.path.basename(program_path)
    elif program_type == 'COBOL':
        # Extract COBOL module name
        program_name = os.path.basename(program_path).replace('.so', '')
    
    logger.info(f"Executing {program_type} program: {program_path} for map: {map_name}")
    
    log_details = {
        'map_name': map_name, 
        'program_type': program_type, 
        'program_path': program_path,
        'program_name': program_name,
        'input_data': input_data
    }
    
    if program_type == 'JAVA' and '.' in program_path:
        log_details['package_name'] = package_name
        log_details['class_name'] = class_name
    
    add_log('INFO', 'EXECUTE', f'Starting {program_type} program: {program_name}', log_details)
    
    try:
        result = multi_executor.execute_program(program_type, program_path, input_data)
        logger.info(f"Program execution successful for map {map_name}")
        
        success_details = {
            'map_name': map_name, 
            'program_type': program_type,
            'program_name': program_name,
            'result': result
        }
        
        if program_type == 'JAVA' and '.' in program_path:
            success_details['package_name'] = package_name
            success_details['class_name'] = class_name
        
        add_log('INFO', 'EXECUTE', f'{program_type} program executed successfully: {program_name}', success_details)
        return result
    
    except Exception as e:
        logger.error(f"Program execution failed for map {map_name}: {e}")
        
        error_details = {
            'map_name': map_name, 
            'program_type': program_type,
            'program_name': program_name,
            'error': str(e)
        }
        
        if program_type == 'JAVA' and '.' in program_path:
            error_details['package_name'] = package_name
            error_details['class_name'] = class_name
        
        add_log('ERROR', 'EXECUTE', f'{program_type} program execution failed: {program_name}', error_details)
        raise

# API ??????

@app.route('/api/health', methods=['GET'])
def health_check():
    """?? ??"""
    return jsonify({
        'status': 'OK',
        'version': 'v0.5.1',
        'smed_dir': SMED_DIR,
        'accounts_loaded': len(accounts),
        'smed_pgm_maps': len(smed_pgm_config),
        'map_pgm_maps': len(map_pgm_config.get('maps', {})),
        'java_available': multi_executor.java_available if multi_executor else False,
        'jar_exists': os.path.exists(multi_executor.jar_path) if multi_executor and multi_executor.jar_path else False
    })

@app.route('/api/smed/<map_name>', methods=['GET'])
def get_smed_map(map_name):
    """SMED ? ?? ??"""
    file_path = get_smed_file_path(map_name)
    
    if not file_path:
        return jsonify({'error': f'SMED file not found: {map_name}'}), 404
    
    smed_data = parse_smed_file(file_path)
    if not smed_data:
        return jsonify({'error': f'Failed to parse SMED file: {map_name}'}), 500
    
    return jsonify(smed_data)

@app.route('/api/smed/logo', methods=['GET'])
def get_logo_map():
    """LOGO ? ?? (?? ?????)"""
    return get_smed_map('LOGO')

@app.route('/api/login', methods=['POST'])
def login():
    """??? ???"""
    data = request.get_json()
    user_id = data.get('user_id')
    password = data.get('password')
    
    if not user_id or not password:
        return jsonify({'error': 'User ID and password required'}), 400
    
    if user_id not in accounts:
        return jsonify({'error': 'Invalid user ID'}), 401
    
    if accounts[user_id].get('password') != password:
        return jsonify({'error': 'Invalid password'}), 401
    
    # ??? ?? ? ??? ???? ?? ??
    user_program = accounts[user_id].get('pgm', 'PGM1')
    
    logger.info(f"User {user_id} logged in successfully")
    add_log('INFO', 'LOGIN', f'User {user_id} logged in successfully', {'user_id': user_id, 'program': user_program})
    
    return jsonify({
        'success': True,
        'message': 'Login successful',
        'user_id': user_id,
        'program': user_program
    })

@app.route('/api/execute', methods=['POST'])
def execute_program():
    """???? ??"""
    data = request.get_json()
    
    user_id = data.get('user_id')
    map_name = data.get('map_name', '')
    input_fields = data.get('input_fields', {})
    
    if not user_id:
        return jsonify({'error': 'User ID required'}), 400
    
    if user_id not in accounts:
        return jsonify({'error': 'Invalid user ID'}), 401
    
    try:
        # ? ?? ???? ??
        if map_name:
            logger.info(f"Executing program for map: {map_name} with input: {input_fields}")
            add_log('INFO', 'EXECUTE', f'Executing program for map: {map_name}', {'user_id': user_id, 'map_name': map_name, 'input_fields': input_fields})
            result = execute_map_program(map_name, input_fields)
            if result:
                add_log('INFO', 'EXECUTE', f'Program execution completed for map: {map_name}', {'user_id': user_id, 'map_name': map_name, 'result': result})
                return jsonify({
                    'success': True,
                    'result': result,
                    'map_name': map_name
                })
        
        # ?? ???? ?? (?? ???)
        program = data.get('program') or accounts[user_id].get('pgm', 'PGM1')
        
        if multi_executor and multi_executor.java_available:
            logger.info(f"Executing default Java program: {program}")
            result = multi_executor.execute_java_program(program, input_fields)
            return jsonify({
                'success': True,
                'result': result,
                'program': program
            })
        else:
            return jsonify({'error': 'Java execution not available'}), 500
    
    except Exception as e:
        logger.error(f"Program execution failed: {e}")
        return jsonify({'error': str(e)}), 500

@app.route('/api/smed_pgm/status', methods=['GET'])
def smed_pgm_status():
    """SMED-PGM ?? ??"""
    return jsonify({
        'loaded': True,
        'maps_count': len(smed_pgm_config),
        'maps': list(smed_pgm_config.keys()),
        'file_path': SMED_PGM_FILE,
        'file_exists': os.path.exists(SMED_PGM_FILE) if SMED_PGM_FILE else False
    })

@app.route('/api/smed_pgm/reload', methods=['GET'])
def reload_smed_pgm():
    """SMED-PGM ?? ???"""
    try:
        load_config()  # ?? ?? ???
        return jsonify({
            'success': True,
            'message': 'SMED-PGM configuration reloaded',
            'maps_count': len(smed_pgm_config)
        })
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/api/smed/files', methods=['GET'])
def get_smed_files():
    """SMEDファイル一覧取得"""
    try:
        if not SMED_DIR or not os.path.exists(SMED_DIR):
            return jsonify({'error': 'SMED directory not found'}), 404
        
        files = []
        for file in os.listdir(SMED_DIR):
            # .smed拡張子があるファイルまたは拡張子なしのファイル（ディレクトリは除外）
            file_path = os.path.join(SMED_DIR, file)
            if os.path.isfile(file_path) and (file.endswith('.smed') or '.' not in file):
                files.append(file)
        
        files.sort()  # ファイル名でソート
        
        return jsonify({
            'success': True,
            'files': files,
            'count': len(files)
        })
    except Exception as e:
        logger.error(f"SMEDファイル一覧取得エラー: {e}")
        return jsonify({'error': str(e)}), 500

@app.route('/api/smed/content/<filename>', methods=['GET'])
def get_smed_content(filename):
    """SMEDファイル内容取得"""
    try:
        if not SMED_DIR or not os.path.exists(SMED_DIR):
            return jsonify({'error': 'SMED directory not found'}), 404
        
        # セキュリティチェック: ファイル名にパストラバーサルが含まれていないか確認
        if '..' in filename or '/' in filename or '\\' in filename:
            return jsonify({'error': 'Invalid filename'}), 400
        
        file_path = os.path.join(SMED_DIR, filename)
        
        if not os.path.exists(file_path):
            return jsonify({'error': 'File not found'}), 404
        
        # ファイル内容を読み取り（Shift_JISエンコーディングを試行）
        try:
            with open(file_path, 'r', encoding='shift_jis') as f:
                content = f.read()
        except UnicodeDecodeError:
            # Shift_JISで読めない場合はUTF-8を試行
            try:
                with open(file_path, 'r', encoding='utf-8') as f:
                    content = f.read()
            except UnicodeDecodeError:
                # それでも読めない場合はバイナリで読んで適当にデコード
                with open(file_path, 'rb') as f:
                    raw_content = f.read()
                    content = raw_content.decode('shift_jis', errors='replace')
        
        return content, 200, {'Content-Type': 'text/plain; charset=utf-8'}
        
    except Exception as e:
        logger.error(f"SMEDファイル内容取得エラー: {e}")
        return jsonify({'error': str(e)}), 500

@app.route('/api/system/info', methods=['GET'])
def get_system_info():
    """システム情報取得"""
    try:
        # CPU情報
        cpu_percent = psutil.cpu_percent(interval=1)
        cpu_count = psutil.cpu_count()
        cpu_freq = psutil.cpu_freq()
        
        # メモリ情報
        memory = psutil.virtual_memory()
        
        # ディスク情報
        disk = psutil.disk_usage('/')
        
        # システム稼働時間
        boot_time = psutil.boot_time()
        uptime_seconds = time.time() - boot_time
        uptime_hours = int(uptime_seconds // 3600)
        uptime_minutes = int((uptime_seconds % 3600) // 60)
        
        return jsonify({
            'success': True,
            'cpu': {
                'usage_percent': round(cpu_percent, 1),
                'core_count': cpu_count,
                'frequency': {
                    'current': round(cpu_freq.current, 1) if cpu_freq else 0,
                    'max': round(cpu_freq.max, 1) if cpu_freq else 0
                }
            },
            'memory': {
                'total': memory.total,
                'available': memory.available,
                'used': memory.used,
                'percent': round(memory.percent, 1),
                'total_gb': round(memory.total / (1024**3), 1),
                'used_gb': round(memory.used / (1024**3), 1),
                'available_gb': round(memory.available / (1024**3), 1)
            },
            'disk': {
                'total': disk.total,
                'used': disk.used,
                'free': disk.free,
                'percent': round((disk.used / disk.total) * 100, 1),
                'total_gb': round(disk.total / (1024**3), 1),
                'used_gb': round(disk.used / (1024**3), 1),
                'free_gb': round(disk.free / (1024**3), 1)
            },
            'uptime': {
                'hours': uptime_hours,
                'minutes': uptime_minutes,
                'formatted': f"{uptime_hours}時間 {uptime_minutes}分"
            }
        })
    except Exception as e:
        logger.error(f"システム情報取得エラー: {e}")
        return jsonify({'error': str(e)}), 500

@app.route('/api/multi_executor/status', methods=['GET'])
def multi_executor_status():
    """Multi-Type Executor ??"""
    if not multi_executor:
        return jsonify({'error': 'Multi-Type Executor not initialized'}), 500
    
    return jsonify({
        'java_available': multi_executor.java_available,
        'jar_path': multi_executor.jar_path,
        'jar_exists': os.path.exists(multi_executor.jar_path) if multi_executor.jar_path else False,
        'cobol_libs_loaded': len(multi_executor.cobol_libs),
        'supported_types': ['JAVA', 'COBOL', 'SHELL']
    })

@app.route('/api/logs', methods=['GET'])
def get_logs():
    """Get execution logs"""
    try:
        logs_list = list(execution_logs)
        return jsonify({
            'success': True,
            'logs': logs_list,
            'count': len(logs_list)
        })
    except Exception as e:
        logger.error(f"Failed to get logs: {e}")
        return jsonify({'error': str(e)}), 500

@app.route('/api/logs', methods=['POST'])
def add_log_entry():
    """Add a new log entry"""
    try:
        data = request.get_json()
        log_entry = {
            'id': str(uuid.uuid4()),
            'timestamp': datetime.now().strftime('%Y-%m-%d %H:%M:%S'),
            'level': data.get('level', 'INFO'),
            'source': data.get('service', 'UNKNOWN'),
            'message': data.get('message', ''),
            'details': data.get('details', {})
        }
        execution_logs.append(log_entry)
        logger.info(f"[{log_entry['level']}] {log_entry['source']}: {log_entry['message']}")
        return jsonify({
            'success': True,
            'message': 'Log entry added successfully',
            'log_id': log_entry['id']
        })
    except Exception as e:
        logger.error(f"Failed to add log entry: {e}")
        return jsonify({'error': str(e)}), 500

@app.route('/api/logs', methods=['DELETE'])
def clear_logs():
    """Clear execution logs"""
    try:
        execution_logs.clear()
        return jsonify({
            'success': True,
            'message': 'Logs cleared successfully'
        })
    except Exception as e:
        logger.error(f"Failed to clear logs: {e}")
        return jsonify({'error': str(e)}), 500

@app.route('/api/smed/parse', methods=['POST'])
def parse_smed_map():
    """Parse SMED map file and return grid layout for web rendering"""
    try:
        data = request.get_json()
        map_file = data.get('map_file')
        
        if not map_file:
            return jsonify({'error': 'map_file parameter required'}), 400
        
        # Parse map file path
        if '/' in map_file:
            lib_name, map_name = map_file.split('/', 1)
        else:
            lib_name = 'TESTLIB'  # Default library
            map_name = map_file
        
        # Add .smed extension if not present
        if not map_name.endswith('.smed'):
            map_name += '.smed'
        
        # Construct full path
        map_path = os.path.join(VOLUME_ROOT, 'DISK01', lib_name, map_name)
        
        if not os.path.exists(map_path):
            return jsonify({'error': f'Map file not found: {map_path}'}), 404
        
        # Parse SMED file
        with open(map_path, 'r', encoding='utf-8') as f:
            smed_content = f.read()
        
        # Initialize 24x80 grid
        grid = [[' ' for _ in range(80)] for _ in range(24)]
        fields = []
        
        # Parse SMED content line by line
        lines = smed_content.strip().split('\n')
        map_name_parsed = None
        
        for line in lines:
            line = line.strip()
            if not line:
                continue
                
            if line.startswith('MAPNAME'):
                map_name_parsed = line.split()[1] if len(line.split()) > 1 else 'UNKNOWN'
            elif line.startswith('ITEM'):
                # Parse ITEM definition
                parts = line.split()
                if len(parts) < 2:
                    continue
                    
                field_name = parts[1]
                field_info = {
                    'name': field_name,
                    'type': 'output',  # Default
                    'row': 0,
                    'col': 0,
                    'length': 0,
                    'value': '',
                    'color': '#FFFFFF',
                    'prompt': ''
                }
                
                # Parse attributes
                for i, part in enumerate(parts[2:]):
                    if part.startswith('TYPE='):
                        field_info['type'] = 'output' if part.split('=')[1] == 'T' else 'input'
                    elif part.startswith('POS='):
                        pos_str = part.split('=')[1]
                        if pos_str.startswith('(') and ')' in pos_str:
                            row, col = pos_str.strip('()').split(',')
                            field_info['row'] = int(row) - 1  # Convert to 0-based
                            field_info['col'] = int(col) - 1
                    elif part.startswith('LEN='):
                        field_info['length'] = int(part.split('=')[1])
                    elif part.startswith('COLOR='):
                        field_info['color'] = part.split('=')[1]
                    elif part.startswith('PROMPT='):
                        # Handle quoted prompt text
                        prompt_start = line.find('PROMPT="') + 8
                        prompt_end = line.find('"', prompt_start)
                        if prompt_start > 7 and prompt_end > prompt_start:
                            field_info['prompt'] = line[prompt_start:prompt_end]
                            field_info['value'] = field_info['prompt']
                
                # If PROMPT exists, it's output mode; if not, it's input mode
                if field_info['prompt']:
                    field_info['type'] = 'output'
                    # Place prompt text on grid
                    text = field_info['prompt']
                    row = field_info['row']
                    col = field_info['col']
                    for j, char in enumerate(text):
                        if col + j < 80 and row < 24:
                            grid[row][col + j] = char
                else:
                    field_info['type'] = 'input'
                    # Set default length if not specified
                    if field_info['length'] == 0:
                        field_info['length'] = 10
                
                fields.append(field_info)
        
        # Convert grid to string representation for easy rendering
        grid_lines = [''.join(row) for row in grid]
        
        return jsonify({
            'success': True,
            'map_name': map_name_parsed,
            'grid': grid_lines,
            'fields': fields,
            'rows': 24,
            'cols': 80
        })
        
    except Exception as e:
        logger.error(f"Failed to parse SMED map: {e}")
        import traceback
        traceback.print_exc()
        return jsonify({'error': str(e)}), 500

@app.route('/api/asp-command', methods=['POST'])
def asp_command():
    """Execute ASP system command"""
    try:
        data = request.get_json()
        
        if not data:
            return jsonify({'error': 'Request body required'}), 400
        
        command = data.get('command', '').strip()
        user = data.get('user', 'unknown')
        
        if not command:
            return jsonify({'error': 'Command parameter required'}), 400
        
        logger.info(f"Received ASP command request: {command} from user: {user}")
        add_log('INFO', 'ASP_COMMAND', f'Executing command: {command}', {'user': user, 'command': command})
        
        # ASP 명령어 실행
        result = execute_asp_command(command, user)
        
        if result['success']:
            add_log('INFO', 'ASP_COMMAND', f'Command completed successfully: {command}', 
                   {'user': user, 'command': command, 'output_length': len(result['output'])})
            return jsonify({
                'success': True,
                'output': result['output'],
                'command': command,
                'user': user
            })
        else:
            add_log('ERROR', 'ASP_COMMAND', f'Command failed: {command}', 
                   {'user': user, 'command': command, 'error': result['error']})
            return jsonify({
                'success': False,
                'error': result['error'],
                'command': command,
                'user': user
            }), 500
    
    except Exception as e:
        error_msg = str(e)
        logger.error(f"ASP command API error: {error_msg}")
        add_log('ERROR', 'ASP_COMMAND', f'API error: {error_msg}', {'error': error_msg})
        return jsonify({'error': error_msg}), 500

def execute_asp_command(command, user):
    """Execute ASP command using aspcli.py"""
    try:
        # ASP 명령어 실행을 위한 스크립트 경로
        script_dir = os.path.dirname(os.path.abspath(__file__))
        aspcli_path = os.path.join(script_dir, 'system-cmds', 'aspcli.py')
        
        if not os.path.exists(aspcli_path):
            raise Exception(f"aspcli.py not found at {aspcli_path}")
        
        # Python 명령어 실행 (UTF-8 인코딩 설정)
        env = os.environ.copy()
        env['PYTHONIOENCODING'] = 'utf-8'
        env['LC_ALL'] = 'C.UTF-8'
        env['LANG'] = 'C.UTF-8'
        
        logger.info(f"Executing ASP command: {command} for user: {user}")
        
        # Parse command to extract command name and parameters
        command_parts = command.strip().split()
        if not command_parts:
            raise Exception("Empty command")
        
        command_name = command_parts[0]
        command_params = command[len(command_name):].strip()
        
        # Build arguments for aspcli.py: command_name [params...]
        cmd_args = [sys.executable, aspcli_path, command_name]
        if command_params:
            cmd_args.append(command_params)
        
        process = subprocess.Popen(
            cmd_args,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=False,  # Use binary mode to avoid encoding issues
            env=env,
            cwd=os.path.dirname(aspcli_path)
        )
        
        stdout_bytes, stderr_bytes = process.communicate(timeout=30)
        
        # Safely decode output using UTF-8 with error handling
        stdout = stdout_bytes.decode('utf-8', errors='replace') if stdout_bytes else ""
        stderr = stderr_bytes.decode('utf-8', errors='replace') if stderr_bytes else ""
        
        if process.returncode != 0:
            error_msg = stderr.strip() if stderr else "Command execution failed"
            logger.error(f"ASP command failed: {error_msg}")
            return {
                'success': False,
                'output': '',
                'error': error_msg
            }
        
        output = stdout.strip()
        logger.info(f"ASP command completed successfully")
        
        return {
            'success': True,
            'output': output,
            'error': None
        }
        
    except subprocess.TimeoutExpired:
        process.kill()
        error_msg = "Command execution timeout"
        logger.error(f"ASP command timeout: {command}")
        return {
            'success': False,
            'output': '',
            'error': error_msg
        }
    except Exception as e:
        error_msg = str(e)
        logger.error(f"ASP command execution error: {error_msg}")
        return {
            'success': False,
            'output': '',
            'error': error_msg
        }

def add_log(level, source, message, details=None):
    """Add log entry"""
    log_entry = {
        'id': str(uuid.uuid4()),
        'timestamp': datetime.now().strftime('%Y-%m-%d %H:%M:%S'),
        'level': level,
        'source': source,
        'message': message,
        'details': details
    }
    execution_logs.append(log_entry)
    logger.info(f"[{level}] {source}: {message}")

if __name__ == '__main__':
    logger.info("OpenASP API Server v0.5.1 starting...")
    logger.info("Enhanced with Multi-Type Program Support (JAVA, COBOL, SHELL)")
    logger.info("Fixed configuration loading and error handling")
    
    # ?? ??
    load_config()
    
    if not SMED_DIR or not os.path.exists(SMED_DIR):
        logger.error(f"SMED directory not found: {SMED_DIR}")
        sys.exit(1)
    
    logger.info(f"? Configuration loaded successfully")
    logger.info(f"?? SMED Files: {len(os.listdir(SMED_DIR)) if os.path.exists(SMED_DIR) else 0}")
    logger.info(f"?? User Accounts: {len(accounts)}")
    logger.info(f"???  SMED-PGM Maps: {len(smed_pgm_config)}")
    logger.info(f"? Java Available: {multi_executor.java_available if multi_executor else False}")
    
    if multi_executor and multi_executor.jar_path:
        if os.path.exists(multi_executor.jar_path):
            logger.info(f"?? JAR File: {multi_executor.jar_path} (EXISTS)")
        else:
            logger.warning(f"?? JAR File: {multi_executor.jar_path} (NOT FOUND)")
    
    logger.info("?? OpenASP SMED API Server ready!")
    
    # ?? ??
    app.run(
        host='0.0.0.0',
        port=8000,
        debug=False,
        threaded=True
    )
