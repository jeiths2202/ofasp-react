#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Flask API service for EBCDIC conversion
NO HARDCODING - all values from configuration
"""

import sys
from pathlib import Path
from flask import Flask, request, jsonify
from flask_cors import CORS
from typing import Dict, Any, Optional
import traceback

# Add project root to Python path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from config.config import config
from src.constants.conversion import ConversionConstants, ValidationRules
from src.converters.ebcdic_converter import converter
from src.utils.logger import api_logger


class ConversionAPI:
    """Flask API service for EBCDIC conversion"""
    
    def __init__(self):
        """Initialize Flask application with configuration"""
        self.app = Flask(__name__)
        self.logger = api_logger
        self._setup_app()
        self._setup_routes()
        
    def _setup_app(self):
        """Setup Flask application with configuration"""
        # CORS configuration
        security_config = config.security_config
        if security_config['cors_origins']:
            CORS(self.app, origins=security_config['cors_origins'])
        else:
            CORS(self.app)
        
        # Application configuration
        self.app.config['JSON_SORT_KEYS'] = False
        self.app.config['JSONIFY_PRETTYPRINT_REGULAR'] = True
        
        self.logger.info("Flask application initialized with configuration")
    
    def _validate_api_key(self, request_data: Dict[str, Any]) -> bool:
        """Validate API key if required"""
        security_config = config.security_config
        
        if not security_config['api_key_required']:
            return True
        
        api_key = request_data.get('api_key') or request.headers.get('X-API-Key')
        return api_key == security_config['api_key']
    
    def _create_error_response(self, error_message: str, status_code: int = 400) -> Dict[str, Any]:
        """Create standardized error response"""
        return {
            'success': False,
            'error': error_message,
            'data': None
        }, status_code
    
    def _create_success_response(self, data: Any, message: str = "Success") -> Dict[str, Any]:
        """Create standardized success response"""
        return {
            'success': True,
            'message': message,
            'data': data
        }
    
    def _setup_routes(self):
        """Setup API routes"""
        
        @self.app.route('/health', methods=['GET'])
        def health_check():
            """Health check endpoint"""
            return self._create_success_response({
                'status': 'healthy',
                'version': ConversionConstants.API_VERSION
            })
        
        @self.app.route('/api/v1/convert/ebcdic-to-ascii', methods=['POST'])
        def convert_ebcdic_to_ascii():
            """Convert EBCDIC to ASCII"""
            try:
                # Parse request data
                data = request.get_json()
                if not data:
                    return self._create_error_response("No JSON data provided")
                
                # Validate API key
                if not self._validate_api_key(data):
                    return self._create_error_response("Invalid API key", 401)
                
                # Extract parameters
                input_data = data.get('input_data')
                encoding = data.get('encoding', config.conversion_config['default_encoding'])
                sosi_flag = data.get('sosi_flag', False)
                out_sosi_flag = data.get('out_sosi_flag', False)
                rlen = data.get('rlen', config.conversion_config['default_record_length'])
                sosi_handling = data.get('sosi_handling', 'remove')
                
                # Validate required parameters
                if not input_data:
                    return self._create_error_response("input_data is required")
                
                # Validate parameters
                if not ValidationRules.is_valid_encoding(encoding):
                    return self._create_error_response(f"Invalid encoding: {encoding}")
                
                if not ValidationRules.is_valid_record_length(rlen):
                    return self._create_error_response(f"Invalid record length: {rlen}")
                
                if isinstance(input_data, str) and not ValidationRules.is_valid_hex_string(input_data):
                    return self._create_error_response("Invalid hex string format")
                
                # Perform conversion
                result = converter.convert_ebcdic_to_ascii(
                    input_data=input_data,
                    encoding=encoding,
                    sosi_flag=sosi_flag,
                    out_sosi_flag=out_sosi_flag,
                    rlen=rlen,
                    sosi_handling=sosi_handling
                )
                
                # Return response
                response_data = {
                    'output': result,
                    'encoding': encoding,
                    'sosi_flag': sosi_flag,
                    'out_sosi_flag': out_sosi_flag,
                    'rlen': rlen,
                    'input_size': len(input_data) if isinstance(input_data, str) else len(input_data.decode('utf-8', errors='ignore')),
                    'output_size': len(result)
                }
                
                self.logger.info(f"EBCDIC to ASCII conversion completed: {len(result)} characters")
                return self._create_success_response(response_data, "EBCDIC to ASCII conversion completed")
                
            except Exception as e:
                self.logger.error(f"Error in EBCDIC to ASCII conversion: {str(e)}")
                self.logger.error(traceback.format_exc())
                return self._create_error_response(f"Conversion failed: {str(e)}", 500)
        
        @self.app.route('/api/v1/convert/ascii-to-ebcdic', methods=['POST'])
        def convert_ascii_to_ebcdic():
            """Convert ASCII to EBCDIC"""
            try:
                # Parse request data
                data = request.get_json()
                if not data:
                    return self._create_error_response("No JSON data provided")
                
                # Validate API key
                if not self._validate_api_key(data):
                    return self._create_error_response("Invalid API key", 401)
                
                # Extract parameters
                input_data = data.get('input_data')
                encoding = data.get('encoding', config.conversion_config['default_encoding'])
                sosi_flag = data.get('sosi_flag', False)
                out_sosi_flag = data.get('out_sosi_flag', False)
                rlen = data.get('rlen', config.conversion_config['default_record_length'])
                sosi_handling = data.get('sosi_handling', 'remove')
                
                # Validate required parameters
                if not input_data:
                    return self._create_error_response("input_data is required")
                
                # Validate parameters
                if not ValidationRules.is_valid_encoding(encoding):
                    return self._create_error_response(f"Invalid encoding: {encoding}")
                
                if not ValidationRules.is_valid_record_length(rlen):
                    return self._create_error_response(f"Invalid record length: {rlen}")
                
                # Perform conversion
                result = converter.convert_ascii_to_ebcdic(
                    input_data=input_data,
                    encoding=encoding,
                    sosi_flag=sosi_flag,
                    out_sosi_flag=out_sosi_flag,
                    rlen=rlen
                )
                
                # Return response
                response_data = {
                    'output': result,
                    'encoding': encoding,
                    'sosi_flag': sosi_flag,
                    'out_sosi_flag': out_sosi_flag,
                    'rlen': rlen,
                    'input_size': len(input_data),
                    'output_size': len(result)
                }
                
                self.logger.info(f"ASCII to EBCDIC conversion completed: {len(result)} hex characters")
                return self._create_success_response(response_data, "ASCII to EBCDIC conversion completed")
                
            except Exception as e:
                self.logger.error(f"Error in ASCII to EBCDIC conversion: {str(e)}")
                self.logger.error(traceback.format_exc())
                return self._create_error_response(f"Conversion failed: {str(e)}", 500)
        
        @self.app.route('/api/v1/convert/sjis-to-unicode', methods=['POST'])
        def convert_sjis_to_unicode():
            """Convert Shift-JIS encoded bytes to Unicode string"""
            try:
                # Parse request data
                data = request.get_json()
                if not data:
                    return self._create_error_response("No JSON data provided")
                
                # Validate API key (optional for internal services)
                if not self._validate_api_key(data):
                    return self._create_error_response("Invalid API key", 401)
                
                # Extract parameters
                input_bytes = data.get('input_bytes')  # Base64 encoded bytes
                encoding = data.get('encoding', 'shift_jis')
                
                # Validate required parameters
                if not input_bytes:
                    return self._create_error_response("input_bytes is required")
                
                # Decode base64 input
                import base64
                try:
                    raw_bytes = base64.b64decode(input_bytes)
                except Exception as e:
                    return self._create_error_response(f"Invalid base64 input: {str(e)}")
                
                # Supported Japanese encodings in priority order
                japanese_encodings = ['cp932', 'shift_jis', 'shift_jisx0213', 'euc-jp']
                
                result_text = None
                used_encoding = None
                
                # Try each encoding
                for enc in japanese_encodings:
                    try:
                        decoded_text = raw_bytes.decode(enc, errors='strict')
                        # Check if decoding was successful (no replacement chars)
                        if '�' not in decoded_text:
                            result_text = decoded_text
                            used_encoding = enc
                            break
                    except (UnicodeDecodeError, UnicodeError):
                        continue
                
                # If strict decoding failed, try with error handling
                if result_text is None:
                    for enc in japanese_encodings:
                        try:
                            decoded_text = raw_bytes.decode(enc, errors='replace')
                            result_text = decoded_text
                            used_encoding = enc + '_with_replacement'
                            break
                        except:
                            continue
                
                if result_text is None:
                    return self._create_error_response("Unable to decode input with any Japanese encoding")
                
                # Create response
                response_data = {
                    'unicode_text': result_text,
                    'used_encoding': used_encoding,
                    'input_size': len(raw_bytes),
                    'output_size': len(result_text),
                    'success': True
                }
                
                self.logger.info(f"SJIS to Unicode conversion completed using {used_encoding}: {len(result_text)} characters")
                return self._create_success_response(response_data, "SJIS to Unicode conversion completed")
                
            except Exception as e:
                self.logger.error(f"Error in SJIS to Unicode conversion: {str(e)}")
                self.logger.error(traceback.format_exc())
                return self._create_error_response(f"Conversion failed: {str(e)}", 500)
        
        @self.app.route('/api/v1/info', methods=['GET'])
        def get_info():
            """Get service information"""
            return self._create_success_response({
                'version': ConversionConstants.API_VERSION,
                'supported_encodings': list(ConversionConstants.SUPPORTED_ENCODINGS),
                'sosi_handling_modes': list(ConversionConstants.SOSI_HANDLING_MODES),
                'error_handling_modes': list(ConversionConstants.ERROR_HANDLING_MODES),
                'max_input_size': config.conversion_config['max_input_size'],
                'default_encoding': config.conversion_config['default_encoding'],
                'default_record_length': config.conversion_config['default_record_length']
            })

        @self.app.route('/scan-directory', methods=['POST'])
        def scan_directory():
            """Scan directory for COBOL, CL, COPYBOOK, and SMED files"""
            import os
            import glob
            
            try:
                data = request.get_json()
                if not data or 'path' not in data:
                    return self._create_error_response('Directory path is required')
                
                directory_path = data['path']
                
                # Validate directory exists
                if not os.path.exists(directory_path):
                    return self._create_error_response(f'Directory does not exist: {directory_path}', 404)
                
                if not os.path.isdir(directory_path):
                    return self._create_error_response(f'Path is not a directory: {directory_path}')
                
                # Scan for supported file types
                supported_extensions = ['*.cob', '*.cobol', '*.cpy', '*.copy', '*.cl', '*.cle', '*.smed', '*.txt']
                files = []
                
                for extension in supported_extensions:
                    pattern = os.path.join(directory_path, '**', extension)
                    matches = glob.glob(pattern, recursive=True)
                    files.extend(matches)
                
                # Also scan files without extensions (common for CL files)
                all_files = glob.glob(os.path.join(directory_path, '**', '*'), recursive=True)
                for file_path in all_files:
                    if os.path.isfile(file_path) and '.' not in os.path.basename(file_path):
                        files.append(file_path)
                
                # Remove duplicates and sort
                files = sorted(list(set(files)))
                
                # Read file contents and classify
                result_files = []
                for file_path in files:
                    try:
                        # Get file info
                        file_stat = os.stat(file_path)
                        file_name = os.path.basename(file_path)
                        
                        # Read file content (limit to first 10KB for classification)
                        with open(file_path, 'rb') as f:
                            raw_content = f.read(10240)  # First 10KB
                        
                        # Try to decode as UTF-8 first, then as SHIFT_JIS
                        try:
                            content = raw_content.decode('utf-8')
                            encoding = 'ASCII'
                        except UnicodeDecodeError:
                            try:
                                content = raw_content.decode('shift_jis')
                                encoding = 'EBCDIC'
                            except UnicodeDecodeError:
                                # Skip binary files
                                continue
                        
                        # Classify file type based on content and extension
                        file_type = self._classify_file_type(file_name, content)
                        
                        result_files.append({
                            'name': file_name,
                            'path': file_path,
                            'type': file_type,
                            'size': file_stat.st_size,
                            'encoding': encoding,
                            'content': content[:1000] if len(content) > 1000 else content  # Limit content preview
                        })
                        
                    except Exception as e:
                        # Log error but continue processing other files
                        self.logger.warning(f"Error processing file {file_path}: {str(e)}")
                        continue
                
                return jsonify({
                    'success': True,
                    'directory': directory_path,
                    'total_files': len(result_files),
                    'files': result_files
                })
                
            except Exception as e:
                self.logger.error(f"Failed to scan directory: {str(e)}")
                return self._create_error_response(f'Failed to scan directory: {str(e)}', 500)
        
        @self.app.errorhandler(404)
        def not_found(error):
            """Handle 404 errors"""
            return self._create_error_response("Endpoint not found", 404)
        
        @self.app.errorhandler(500)
        def internal_error(error):
            """Handle 500 errors"""
            self.logger.error(f"Internal server error: {str(error)}")
            return self._create_error_response("Internal server error", 500)
    
    def _classify_file_type(self, filename: str, content: str) -> str:
        """Classify file type based on filename and content"""
        ext = filename.lower().split('.')[-1] if '.' in filename else ''
        upper_content = content.upper()
        
        # COBOL files
        if ext in ['cob', 'cobol'] or 'IDENTIFICATION DIVISION' in upper_content:
            return 'COBOL'
        
        # COPYBOOK files
        if ext in ['cpy', 'copy'] or 'COPY ' in upper_content or '01 ' in upper_content:
            return 'COPYBOOK'
        
        # CL files
        if ext in ['cl', 'cle'] or 'PGM (' in upper_content or 'CALL PGM-' in upper_content or 'DEFLIBR' in upper_content:
            return 'CL'
        
        # SMED files
        if ext == 'smed' or 'SMED' in upper_content:
            return 'SMED'
        
        return 'UNKNOWN'
    
    def run(self):
        """Run the Flask application"""
        flask_config = config.flask_config
        
        self.logger.info(f"Starting conversion service on {flask_config['host']}:{flask_config['port']}")
        
        self.app.run(
            host=flask_config['host'],
            port=flask_config['port'],
            debug=flask_config['debug']
        )


# Create global API instance
api = ConversionAPI()

# Export for easy import
__all__ = ['ConversionAPI', 'api']