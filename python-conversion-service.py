#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Python EBCDIC Conversion Service API
포트 3003에서 실행되는 EBCDIC/ASCII 변환 전용 서비스
"""

import os
import sys
from pathlib import Path
from flask import Flask, request, jsonify
from flask_cors import CORS
import logging

# Add project root to path
project_root = Path(__file__).parent
sys.path.insert(0, str(project_root))

# Import EBCDIC converter
try:
    # Try different import paths
    sys.path.insert(0, str(project_root / "ofasp-refactor" / "src" / "utils"))
    from ebcdic_batch_converter import EbcdicBatchConverter
    converter = EbcdicBatchConverter()
    print("EBCDIC Batch Converter loaded successfully")
except ImportError as e:
    print(f"Failed to import EBCDIC converter: {e}")
    print("Available files in utils directory:")
    utils_dir = project_root / "ofasp-refactor" / "src" / "utils"
    if utils_dir.exists():
        for f in utils_dir.glob("*.py"):
            print(f"  {f.name}")
    converter = None

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s [%(levelname)s] %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger(__name__)

app = Flask(__name__)
CORS(app, origins=[
    'http://localhost:3005', 
    'http://localhost:3000', 
    'http://localhost:3007'
])

@app.route('/health', methods=['GET'])
def health_check():
    """Health check endpoint"""
    return jsonify({
        'success': True,
        'message': 'Python EBCDIC Conversion Service is running',
        'converter_available': converter is not None
    })

@app.route('/api/v1/info', methods=['GET'])
def get_info():
    """Get service information"""
    return jsonify({
        'success': True,
        'data': {
            'version': '1.0.0',
            'supported_encodings': ['US', 'JP', 'KR', 'JAK', 'KEIS'],
            'sosi_handling_modes': ['remove', 'keep', 'space'],
            'error_handling_modes': ['replace', 'ignore', 'strict'],
            'max_input_size': 1024 * 1024,  # 1MB
            'default_encoding': 'US',
            'default_record_length': 80
        }
    })

@app.route('/api/v1/convert/ebcdic-to-ascii', methods=['POST'])
def convert_ebcdic_to_ascii():
    """Convert EBCDIC to ASCII"""
    try:
        if not converter:
            return jsonify({
                'success': False,
                'error': 'EBCDIC converter not available'
            }), 500

        data = request.get_json()
        if not data:
            return jsonify({
                'success': False,
                'error': 'No JSON data provided'
            }), 400

        # Required parameters
        input_data = data.get('input_data')
        if not input_data:
            return jsonify({
                'success': False,
                'error': 'input_data is required'
            }), 400

        # Optional parameters with defaults
        encoding = data.get('encoding', 'US')
        sosi_flag = data.get('sosi_flag', False)
        out_sosi_flag = data.get('out_sosi_flag', False)
        rlen = data.get('rlen', len(input_data) // 2 if isinstance(input_data, str) else 80)
        sosi_handling = data.get('sosi_handling', 'remove')
        
        # Custom SOSI codes (if provided by client)
        sosi_so = data.get('sosi_so')  # Shift-Out code
        sosi_si = data.get('sosi_si')  # Shift-In code
        
        # Convert hex string to int if provided
        if sosi_so is not None:
            if isinstance(sosi_so, str):
                sosi_so = int(sosi_so, 16)
        if sosi_si is not None:
            if isinstance(sosi_si, str):
                sosi_si = int(sosi_si, 16)

        logger.info(f"Converting EBCDIC to ASCII: encoding={encoding}, sosi_flag={sosi_flag}, rlen={rlen}")
        if sosi_so is not None or sosi_si is not None:
            logger.info(f"Custom SOSI codes: SO=0x{sosi_so:02X} SI=0x{sosi_si:02X}" if sosi_so and sosi_si else f"Custom SOSI codes: SO={sosi_so} SI={sosi_si}")

        # Handle SOSI handling parameter
        if sosi_handling == 'space':
            out_sosi_flag = False  # Convert SOSI codes to spaces
        elif sosi_handling == 'keep':
            out_sosi_flag = True   # Keep SOSI codes in output
        else:  # remove
            out_sosi_flag = False  # Remove SOSI codes

        # Perform conversion
        try:
            result = converter.EBCDIC_TO_ASCII(
                input_data=input_data,
                output_buffer=None,
                encoding=encoding,
                sosi_flag=sosi_flag,
                out_sosi_flag=out_sosi_flag,
                rlen=rlen,
                layout=None,
                sosi_so=sosi_so,
                sosi_si=sosi_si
            )

            # Handle SOSI space conversion
            if sosi_handling == 'space' and sosi_flag:
                # Replace SOSI codes with spaces
                result = result.replace('\x0E', ' ').replace('\x0F', ' ')

            # Ensure result is safe for JSON serialization
            try:
                # Test if result can be JSON serialized
                import json
                json.dumps(result)
            except (UnicodeDecodeError, UnicodeEncodeError) as e:
                logger.warning(f"Result contains non-serializable characters: {e}")
                # Replace problematic characters with safe alternatives
                result = result.encode('utf-8', errors='replace').decode('utf-8')

            return jsonify({
                'success': True,
                'message': 'Conversion completed successfully',
                'data': {
                    'output': result,
                    'encoding': encoding,
                    'sosi_flag': sosi_flag,
                    'out_sosi_flag': out_sosi_flag,
                    'rlen': rlen,
                    'input_size': len(input_data) if isinstance(input_data, str) else 0,
                    'output_size': len(result)
                }
            })

        except Exception as e:
            logger.error(f"Conversion error: {e}")
            return jsonify({
                'success': False,
                'error': f'Conversion failed: {str(e)}'
            }), 500

    except Exception as e:
        logger.error(f"Request processing error: {e}")
        return jsonify({
            'success': False,
            'error': f'Request processing failed: {str(e)}'
        }), 500

@app.route('/api/v1/convert/ascii-to-ebcdic', methods=['POST'])
def convert_ascii_to_ebcdic():
    """Convert ASCII to EBCDIC"""
    try:
        if not converter:
            return jsonify({
                'success': False,
                'error': 'EBCDIC converter not available'
            }), 500

        data = request.get_json()
        if not data:
            return jsonify({
                'success': False,
                'error': 'No JSON data provided'
            }), 400

        # Required parameters
        input_data = data.get('input_data')
        if not input_data:
            return jsonify({
                'success': False,
                'error': 'input_data is required'
            }), 400

        # Optional parameters with defaults
        encoding = data.get('encoding', 'US')
        sosi_flag = data.get('sosi_flag', False)
        out_sosi_flag = data.get('out_sosi_flag', False)
        rlen = data.get('rlen', len(input_data))

        logger.info(f"Converting ASCII to EBCDIC: encoding={encoding}, sosi_flag={sosi_flag}, rlen={rlen}")

        # Perform conversion
        try:
            result = converter.ASCII_TO_EBCDIC(
                input_data=input_data,
                output_buffer=None,
                encoding=encoding,
                sosi_flag=sosi_flag,
                out_sosi_flag=out_sosi_flag,
                rlen=rlen,
                layout=None
            )

            return jsonify({
                'success': True,
                'message': 'Conversion completed successfully',
                'data': {
                    'output': result,
                    'encoding': encoding,
                    'sosi_flag': sosi_flag,
                    'out_sosi_flag': out_sosi_flag,
                    'rlen': rlen,
                    'input_size': len(input_data),
                    'output_size': len(result)
                }
            })

        except Exception as e:
            logger.error(f"Conversion error: {e}")
            return jsonify({
                'success': False,
                'error': f'Conversion failed: {str(e)}'
            }), 500

    except Exception as e:
        logger.error(f"Request processing error: {e}")
        return jsonify({
            'success': False,
            'error': f'Request processing failed: {str(e)}'
        }), 500

if __name__ == '__main__':
    port = int(os.environ.get('FLASK_PORT', 3003))
    
    print("=" * 60)
    print("Python EBCDIC Conversion Service")
    print("=" * 60)
    print(f"Starting on port {port}")
    print(f"Converter available: {converter is not None}")
    print("Endpoints:")
    print("  GET  /health")
    print("  GET  /api/v1/info")
    print("  POST /api/v1/convert/ebcdic-to-ascii")
    print("  POST /api/v1/convert/ascii-to-ebcdic")
    print("=" * 60)
    
    app.run(host='0.0.0.0', port=port, debug=True)