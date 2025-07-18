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
        
        @self.app.errorhandler(404)
        def not_found(error):
            """Handle 404 errors"""
            return self._create_error_response("Endpoint not found", 404)
        
        @self.app.errorhandler(500)
        def internal_error(error):
            """Handle 500 errors"""
            self.logger.error(f"Internal server error: {str(error)}")
            return self._create_error_response("Internal server error", 500)
    
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