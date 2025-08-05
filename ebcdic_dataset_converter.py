#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
OpenASP EBCDIC Dataset Converter
Converts EBCDIC datasets to ASCII according to catalog.json specifications
"""

import os
import sys
import json
import struct
import logging
from datetime import datetime
from typing import Dict, List, Optional, Tuple, Any, Union
from pathlib import Path
import re

# Configure logging
logging.basicConfig(
    level=logging.INFO,  
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('/tmp/ebcdic_dataset_converter.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class CatalogManager:
    """Catalog.json management for OpenASP system"""
    
    def __init__(self, catalog_path: str = "/home/aspuser/app/config/catalog.json"):
        self.catalog_path = catalog_path
        self.catalog_data = self._load_catalog()
    
    def _load_catalog(self) -> Dict:
        """Load catalog.json"""
        try:
            with open(self.catalog_path, 'r', encoding='utf-8') as f:
                return json.load(f)
        except Exception as e:
            logger.error(f"Failed to load catalog: {e}")
            return {}
    
    def save_catalog(self) -> bool:
        """Save catalog.json"""
        try:
            with open(self.catalog_path, 'w', encoding='utf-8') as f:
                json.dump(self.catalog_data, f, indent=2, ensure_ascii=False)
            logger.info(f"Catalog saved: {self.catalog_path}")
            return True
        except Exception as e:
            logger.error(f"Failed to save catalog: {e}")
            return False
    
    def get_layout_info(self, layout_name: str) -> Optional[Dict]:
        """Get layout information from catalog"""
        try:
            return self.catalog_data["DISK01"]["LAYOUT"].get(layout_name)
        except KeyError:
            logger.error(f"Layout {layout_name} not found in catalog")
            return None
    
    def register_converted_dataset(self, dataset_name: str, conversion_info: Dict, volume: str = "DISK01", library: str = "TESTLIB") -> bool:
        """Register converted dataset in catalog with specified volume and library"""
        try:
            # Ensure volume exists
            if volume not in self.catalog_data:
                self.catalog_data[volume] = {}
            
            # Ensure library exists in volume
            if library not in self.catalog_data[volume]:
                self.catalog_data[volume][library] = {}
            
            # Create dataset entry
            self.catalog_data[volume][library][dataset_name] = {
                "TYPE": "DATASET",
                "RECTYPE": "FB",
                "RECLEN": conversion_info.get("record_length", 80),
                "ENCODING": conversion_info.get("target_encoding", "ascii"),
                "DESCRIPTION": f"Converted from EBCDIC ({conversion_info.get('source_encoding', 'JAK')})",
                "UPDATED": datetime.now().isoformat() + 'Z',
                "CONVERSION": conversion_info
            }
            
            logger.info(f"Dataset registered: {volume}.{library}.{dataset_name}")
            return self.save_catalog()
        except Exception as e:
            logger.error(f"Failed to register dataset: {e}")
            return False

class LayoutParser:
    """COBOL layout file parser for OpenASP system"""
    
    def __init__(self):
        self.fields = []
    
    def load_json_schema(self, schema_file: str) -> List[Dict]:
        """Load field layout from JSON schema file"""
        try:
            with open(schema_file, 'r', encoding='utf-8') as f:
                schema_data = json.load(f)
            
            fields = []
            current_position = 1
            
            # Support different JSON schema formats
            if 'copybook_analysis' in schema_data:
                # Handle copybook analysis format
                field_definitions = self._extract_fields_from_copybook(schema_data['copybook_analysis'])
            elif 'fields' in schema_data:
                field_definitions = schema_data['fields']
            elif isinstance(schema_data, list):
                field_definitions = schema_data
            else:
                raise ValueError("Invalid JSON schema format")
            
            for field_def in field_definitions:
                field = {
                    'name': field_def.get('name', f'FIELD_{len(fields) + 1}'),
                    'level': field_def.get('level', 3),
                    'type': field_def.get('type', 'DISPLAY'),
                    'picture': field_def.get('picture', field_def.get('pic', 'X(10)')),
                    'length': field_def.get('length', field_def.get('size', self._calculate_field_length(field_def.get('picture', field_def.get('pic', 'X(10)')), field_def.get('type', 'DISPLAY')))),
                    'position': field_def.get('position', current_position),
                    'description': field_def.get('description', '')
                }
                
                # Update position for next field if not explicitly specified
                if 'position' not in field_def:
                    current_position += field['length']
                else:
                    current_position = field_def['position'] + field['length']
                
                fields.append(field)
            
            logger.info(f"JSON schema loaded: {len(fields)} fields from {schema_file}")
            self.fields = fields
            return fields
            
        except Exception as e:
            logger.error(f"Failed to load JSON schema {schema_file}: {e}")
            raise
    
    def _extract_fields_from_copybook(self, copybook_data: Dict) -> List[Dict]:
        """Extract elementary fields from copybook analysis format"""
        fields = []
        
        def extract_elementary_fields(field_list):
            for field in field_list:
                if field.get('type') == 'elementary':
                    # Extract PIC clause from pic field, removing trailing period
                    pic_clause = field.get('pic', '').rstrip('.')
                    fields.append({
                        'name': field.get('name'),
                        'level': field.get('level', 3),
                        'type': 'DISPLAY',  # Default to DISPLAY
                        'picture': pic_clause,
                        'size': field.get('size'),
                        'usage': field.get('usage', 'DISPLAY')
                    })
                elif field.get('children'):
                    # Recursively process children
                    extract_elementary_fields(field['children'])
        
        if 'fields' in copybook_data:
            extract_elementary_fields(copybook_data['fields'])
        
        return fields
    
    def parse_layout_file(self, layout_path: str) -> List[Dict]:
        """Parse COBOL layout file"""
        if not os.path.exists(layout_path):
            raise FileNotFoundError(f"Layout file not found: {layout_path}")
        
        logger.info(f"Parsing layout file: {layout_path}")
        
        try:
            # Try SJIS encoding first (OpenASP standard)
            with open(layout_path, 'r', encoding='shift_jis') as f:
                content = f.read()
        except UnicodeDecodeError:
            # Fallback to UTF-8
            with open(layout_path, 'r', encoding='utf-8') as f:
                content = f.read()
        
        self.fields = []
        current_position = 1
        
        for line_num, line in enumerate(content.split('\n'), 1):
            # Skip comments and empty lines
            if line.strip().startswith('*') or line.strip() == '':
                continue
            
            # Parse COBOL field definition
            field = self._parse_field_line(line, current_position)
            if field:
                self.fields.append(field)
                current_position += field['length']
                logger.debug(f"Parsed field: {field['name']} at position {field['position']} length {field['length']}")
        
        if not self.fields:
            raise ValueError("No fields found in layout file")
        
        logger.info(f"Layout parsed successfully: {len(self.fields)} fields, total length: {current_position - 1}")
        return self.fields
    
    def _parse_field_line(self, line: str, position: int) -> Optional[Dict]:
        """Parse single COBOL field line"""
        line = line.strip()
        if not line or line.startswith('*'):
            return None
            
        # Simple parsing approach for better reliability
        parts = line.split()
        if len(parts) < 4:
            return None
            
        # Look for level number, field name, PIC keyword, and picture clause
        try:
            level_num = int(parts[0])
            field_name = parts[1]
            
            # Find PIC keyword
            pic_index = -1
            for i, part in enumerate(parts):
                if part.upper() == 'PIC':
                    pic_index = i
                    break
            
            if pic_index == -1 or pic_index + 1 >= len(parts):
                return None
                
            picture = parts[pic_index + 1].rstrip('.')
            
            # Skip group level items (01, 02 levels) unless they have PIC clause
            if level_num <= 2 and pic_index == -1:
                return None
            
            # Determine field type and calculate length
            field_type = 'DISPLAY'
            comp_index = -1
            for i, part in enumerate(parts):
                if part.upper().startswith('COMP'):
                    comp_index = i
                    if 'COMP-3' in part.upper():
                        field_type = 'COMP-3'
                    else:
                        field_type = 'COMP'
                    break
            
            length = self._calculate_field_length(picture, field_type)
            
            return {
                'name': field_name,
                'level': level_num,
                'type': field_type,
                'picture': picture,
                'length': length,
                'position': position,
                'description': ''
            }
            
        except (ValueError, IndexError):
            return None
    
    def _calculate_field_length(self, picture: str, field_type: str) -> int:
        """Calculate field length from PIC clause"""
        picture = picture.upper().strip()
        
        # Handle parentheses notation: 9(5), X(20), etc.
        if '(' in picture and ')' in picture:
            # Extract all numbers in parentheses
            total_length = 0
            for match in re.finditer(r'[X9SNVZ]\((\d+)\)', picture):
                total_length += int(match.group(1))
            
            # If no matches found, try simple parentheses
            if total_length == 0:
                length_match = re.search(r'\((\d+)\)', picture)
                if length_match:
                    total_length = int(length_match.group(1))
                else:
                    total_length = 1
            base_length = total_length
        else:
            # Count consecutive characters
            total_length = 0
            i = 0
            while i < len(picture):
                char = picture[i]
                if char in 'X9SNVZ':
                    count = 1
                    while i + count < len(picture) and picture[i + count] == char:
                        count += 1
                    total_length += count
                    i += count
                else:
                    i += 1
            base_length = max(1, total_length)
        
        # Apply field type-specific calculations
        if field_type == 'COMP-3':
            # Packed decimal: (digits + 1) / 2
            return (base_length + 1) // 2
        elif field_type == 'COMP':
            # Binary: depends on digits
            if base_length <= 4:
                return 2
            elif base_length <= 9:
                return 4
            else:
                return 8
        else:
            # DISPLAY: 1 byte per character 
            return base_length

class EBCDICConverter:
    """EBCDIC to ASCII converter for OpenASP system"""
    
    def __init__(self):
        # JAK (Fujitsu Japanese) EBCDIC conversion table
        self.jak_table = self._create_jak_table()
        
        # Code page tables cache
        self.codepage_cache = {}
        self.codepage_base_path = self._get_codepage_base_path()
        
        self.conversion_stats = {
            'total_records': 0,
            'successful_records': 0,
            'error_records': 0,
            'conversion_errors': []
        }
    
    def _create_jak_table(self) -> Dict[int, str]:
        """Create JAK (Fujitsu Japanese) EBCDIC conversion table with DBCS support"""
        table = {}
        
        # Basic ASCII mappings for JAK
        ascii_mappings = {
            0x40: ' ',   # Space
            0x4B: '.',   # Period
            0x4C: '<',   # Less than
            0x4D: '(',   # Left parenthesis
            0x4E: '+',   # Plus
            0x4F: '|',   # Vertical bar
            0x50: '&',   # Ampersand
            0x5A: '!',   # Exclamation mark
            0x5B: '$',   # Dollar sign
            0x5C: '*',   # Asterisk
            0x5D: ')',   # Right parenthesis  
            0x5E: ';',   # Semicolon
            0x60: '-',   # Hyphen/minus
            0x61: '/',   # Slash
            0x6B: ',',   # Comma
            0x6C: '%',   # Percent
            0x6D: '_',   # Underscore
            0x6E: '>',   # Greater than
            0x6F: '?',   # Question mark
            0x7A: ':',   # Colon
            0x7B: '#',   # Number sign
            0x7C: '@',   # At sign
            0x7D: "'",   # Apostrophe
            0x7E: '=',   # Equal sign
            0x7F: '"',   # Quotation mark
        }
        table.update(ascii_mappings)
        
        # Letters A-I
        for i, char in enumerate('abcdefghi'):
            table[0x81 + i] = char
        for i, char in enumerate('ABCDEFGHI'):
            table[0xC1 + i] = char
            
        # Letters J-R
        for i, char in enumerate('jklmnopqr'):
            table[0x91 + i] = char
        for i, char in enumerate('JKLMNOPQR'):
            table[0xD1 + i] = char
            
        # Letters S-Z
        for i, char in enumerate('stuvwxyz'):
            table[0xA2 + i] = char
        for i, char in enumerate('STUVWXYZ'):
            table[0xE2 + i] = char
            
        # Numbers 0-9
        for i in range(10):
            table[0xF0 + i] = str(i)
        
        # SOSI codes for DBCS mode switching
        table[0x0E] = '\x0E'  # SO (Shift Out) - Start DBCS
        table[0x0F] = '\x0F'  # SI (Shift In) - End DBCS
        
        return table
    
    def _get_codepage_base_path(self) -> str:
        """Get codepage base path from environment or default"""
        # Try multiple possible paths for codepage files
        possible_paths = [
            os.environ.get('CODEPAGE_BASE_PATH'),
            '/home/aspuser/app/ofasp-refactor/public/codepages',
            '/home/aspuser/app/public/codepages',
            '/home/aspuser/app/build/codepages'
        ]
        
        for path in possible_paths:
            if path and os.path.exists(path):
                logger.info(f"Using codepage base path: {path}")
                return path
        
        # Default fallback
        default_path = '/home/aspuser/app/ofasp-refactor/public/codepages'
        logger.warning(f"No codepage path found, using default: {default_path}")
        return default_path
    
    def _load_jak_dbcs_mapping(self, jak_code: int) -> Optional[int]:
        """Load JAK EBCDIC DBCS to JEF mapping from code page file"""
        try:
            if 'JEFASCK_DBCS' not in self.codepage_cache:
                self._load_codepage_table('JEFASCK.txt')
            
            dbcs_table = self.codepage_cache.get('JEFASCK_DBCS', {})
            return dbcs_table.get(jak_code, 0)
            
        except Exception as e:
            logger.error(f"Failed to load JAK DBCS mapping for {jak_code:04X}: {e}")
            return None
    
    def _load_codepage_table(self, filename: str):
        """Load code page table from file"""
        filepath = os.path.join(self.codepage_base_path, filename)
        
        if not os.path.exists(filepath):
            raise FileNotFoundError(f"Code page file not found: {filepath}")
        
        logger.info(f"Loading code page table: {filepath}")
        
        single_byte_table = {}
        double_byte_table = {}
        is_double_byte_section = False
        
        try:
            with open(filepath, 'r', encoding='utf-8') as f:
                for line_num, line in enumerate(f, 1):
                    line = line.strip()
                    
                    if line == '[Double byte mapping table]':
                        is_double_byte_section = True
                        continue
                    elif line == '[Single byte mapping table]':
                        is_double_byte_section = False
                        continue
                    elif line.startswith('[') or not line:
                        continue
                    
                    # Parse mapping lines like "4040 - 8140"
                    match = re.match(r'^([0-9A-Fa-f]+)\s*-\s*([0-9A-Fa-f]+)$', line)
                    if match:
                        from_code = int(match.group(1), 16)
                        to_code = int(match.group(2), 16)
                        
                        if is_double_byte_section:
                            double_byte_table[from_code] = to_code
                        else:
                            single_byte_table[from_code] = to_code
        
            # Cache the loaded tables
            base_name = filename.replace('.txt', '')
            self.codepage_cache[f'{base_name}_SBCS'] = single_byte_table
            self.codepage_cache[f'{base_name}_DBCS'] = double_byte_table
            
            logger.info(f"Loaded {len(single_byte_table)} single-byte and {len(double_byte_table)} double-byte mappings from {filename}")
            
        except Exception as e:
            logger.error(f"Failed to load code page table {filepath}: {e}")
            raise
    
    def convert_display_field(self, data: bytes, sosi_config: Optional[Dict] = None) -> str:
        """Convert DISPLAY field data with SOSI handling"""
        if sosi_config:
            return self._convert_with_sosi(data, sosi_config)
        else:
            # Basic conversion without SOSI processing
            result = []
            for byte_val in data:
                char = self.jak_table.get(byte_val, f'\\x{byte_val:02X}')
                result.append(char)
            return ''.join(result).rstrip()
    
    def _convert_with_sosi(self, data: bytes, sosi_config: Dict) -> str:
        """Convert data with SOSI (Shift Out/Shift In) processing"""
        so_code = sosi_config.get('so_code', 0x0E)
        si_code = sosi_config.get('si_code', 0x0F)
        sosi_handling = sosi_config.get('sosi_handling', 'SPACE').upper()
        japanese_encoding = sosi_config.get('japanese_encoding', 'utf-8')
        
        result = []
        i = 0
        dbcs_mode = False
        
        while i < len(data):
            byte_val = data[i]
            
            if byte_val == so_code:
                # Shift Out - start DBCS mode
                dbcs_mode = True
                if sosi_handling == 'SOSI':
                    result.append(chr(so_code))
                elif sosi_handling == 'SPACE':
                    result.append(' ')
                # For REMOVE mode, add nothing
                i += 1
                
            elif byte_val == si_code:
                # Shift In - return to SBCS mode
                dbcs_mode = False
                if sosi_handling == 'SOSI':
                    result.append(chr(si_code))
                elif sosi_handling == 'SPACE':
                    result.append(' ')
                # For REMOVE mode, add nothing
                i += 1
                
            elif dbcs_mode and i + 1 < len(data):
                # DBCS mode - process 2-byte characters
                dbcs_char = self._convert_dbcs_char(data[i], data[i + 1], japanese_encoding)
                result.append(dbcs_char)
                i += 2
                
            else:
                # SBCS mode - process 1-byte characters
                char = self.jak_table.get(byte_val, f'\\x{byte_val:02X}')
                result.append(char)
                i += 1
        
        return ''.join(result).rstrip()
    
    def _convert_dbcs_char(self, high_byte: int, low_byte: int, japanese_encoding: str = 'utf-8') -> str:
        """Convert 2-byte JAK EBCDIC DBCS character using code page table"""
        try:
            # Calculate DBCS character code
            dbcs_code = (high_byte << 8) | low_byte
            
            # Load JAK DBCS conversion table from code page file
            jef_code = self._load_jak_dbcs_mapping(dbcs_code)
            
            if jef_code and jef_code != 0:
                # Convert JEF code to Shift_JIS character
                try:
                    if jef_code == 0x8140:
                        # Full-width space
                        unicode_char = '\u3000'
                    else:
                        # Convert JEF code to Shift_JIS bytes
                        high_jef = (jef_code >> 8) & 0xFF
                        low_jef = jef_code & 0xFF
                        sjis_bytes = bytes([high_jef, low_jef])
                        unicode_char = sjis_bytes.decode('shift_jis', errors='replace')
                    
                    if japanese_encoding.lower() == 'sjis':
                        try:
                            # Return as Shift_JIS
                            return unicode_char.encode('shift_jis', errors='replace').decode('shift_jis', errors='replace')
                        except UnicodeEncodeError:
                            return unicode_char
                    else:
                        # Return as UTF-8 (default)
                        return unicode_char
                        
                except Exception as e:
                    logger.warning(f"JEF to Shift_JIS conversion failed for {jef_code:04X}: {e}")
                    return f'\\u{jef_code:04X}'
            
            # If no mapping found, express as hexadecimal
            return f'\\u{dbcs_code:04X}'
            
        except Exception as e:
            logger.warning(f"JAK EBCDIC DBCS conversion failed for {high_byte:02X}{low_byte:02X}: {e}")
            return f'\\x{high_byte:02X}\\x{low_byte:02X}'
    
    def _is_japanese_dbcs_range(self, code: int) -> bool:
        """Check if code is in Japanese DBCS range"""
        # Shift_JIS zone 1 (0x8140-0x9FFC)
        if 0x8140 <= code <= 0x9FFC:
            return True
        # Shift_JIS zone 2 (0xE040-0xEFFC)
        if 0xE040 <= code <= 0xEFFC:
            return True
        return False
    
    def convert_comp_field(self, data: bytes) -> int:
        """Convert COMP (binary) field data"""
        try:
            if len(data) == 2:
                return struct.unpack('>H', data)[0]  # Big-endian unsigned short
            elif len(data) == 4:
                return struct.unpack('>I', data)[0]  # Big-endian unsigned int
            elif len(data) == 8:
                return struct.unpack('>Q', data)[0]  # Big-endian unsigned long long
            else:
                return 0
        except struct.error:
            return 0
    
    def convert_comp3_field(self, data: bytes) -> str:
        """Convert COMP-3 (packed decimal) field data"""
        if not data:
            return "0"
        
        try:
            # Extract digits from packed decimal
            digits = []
            for i, byte in enumerate(data):
                high_nibble = (byte >> 4) & 0x0F
                low_nibble = byte & 0x0F
                
                if i == len(data) - 1:
                    # Last byte: high nibble is digit, low nibble is sign
                    if high_nibble <= 9:
                        digits.append(str(high_nibble))
                    sign = '+' if low_nibble in [0x0C, 0x0F] else '-'
                else:
                    # Regular byte: both nibbles are digits
                    if high_nibble <= 9:
                        digits.append(str(high_nibble))
                    if low_nibble <= 9:
                        digits.append(str(low_nibble))
            
            if not digits:
                return "0"
            
            number_str = ''.join(digits)
            return sign + number_str if sign == '-' else number_str
            
        except Exception:
            return f"COMP3_ERROR[{data.hex()}]"
    
    def convert_field_data(self, data: bytes, field: Dict, sosi_config: Optional[Dict] = None) -> Any:
        """Convert field data according to its type"""
        field_type = field['type']
        
        if field_type == 'DISPLAY':
            return self.convert_display_field(data, sosi_config)
        elif field_type == 'COMP':
            return self.convert_comp_field(data)
        elif field_type == 'COMP-3':
            return self.convert_comp3_field(data)
        else:
            return self.convert_display_field(data, sosi_config)
    
    def convert_record(self, record_data: bytes, fields: List[Dict], sosi_config: Optional[Dict] = None) -> Dict[str, Any]:
        """Convert single EBCDIC record"""
        result = {}
        
        for field in fields:
            field_start = field['position'] - 1  # Convert to 0-based index
            field_end = field_start + field['length']
            
            if field_end > len(record_data):
                # Handle truncated data
                field_data = record_data[field_start:] + b'\x00' * (field['length'] - (len(record_data) - field_start))
            else:
                field_data = record_data[field_start:field_end]
            
            try:
                converted_value = self.convert_field_data(field_data, field, sosi_config)
                result[field['name']] = converted_value
            except Exception as e:
                self.conversion_stats['conversion_errors'].append(
                    f"Field {field['name']} conversion error: {e}"
                )
                result[field['name']] = f"ERROR[{field_data.hex()}]"
        
        return result

class OpenASPDatasetConverter:
    """Main converter class for OpenASP EBCDIC datasets"""
    
    def __init__(self):
        self.catalog_manager = CatalogManager()
        self.layout_parser = LayoutParser()
        self.ebcdic_converter = EBCDICConverter()
    
    def convert_dataset(self, input_file: str, output_file: str, layout_file: str,
                       output_dataset_name: str = None, output_format: str = 'json',
                       sosi_config: Optional[Dict] = None, volume: str = "DISK01", 
                       library: str = "TESTLIB", schema_file: Optional[str] = None) -> bool:
        """Convert EBCDIC dataset to ASCII"""
        try:
            logger.info("=== OpenASP EBCDIC Dataset Conversion ===")
            logger.info(f"Input file: {input_file}")
            logger.info(f"Output file: {output_file}")  
            logger.info(f"Layout file: {layout_file}")
            
            # Parse layout file or JSON schema
            if schema_file:
                logger.info(f"Using JSON schema: {schema_file}")
                fields = self.layout_parser.load_json_schema(schema_file)
            else:
                logger.info(f"Using COBOL layout: {layout_file}")
                fields = self.layout_parser.parse_layout_file(layout_file)
            record_length = max(field['position'] + field['length'] - 1 for field in fields)
            
            logger.info(f"Record length: {record_length} bytes")
            logger.info(f"Fields count: {len(fields)}")
            
            # Read EBCDIC data
            if not os.path.exists(input_file):
                raise FileNotFoundError(f"Input file not found: {input_file}")
            
            with open(input_file, 'rb') as f:
                ebcdic_data = f.read()
            
            logger.info(f"Input file size: {len(ebcdic_data)} bytes")
            
            # Calculate record count
            if len(ebcdic_data) % record_length != 0:
                logger.warning(f"File size is not multiple of record length")
                logger.warning(f"File size: {len(ebcdic_data)}, Record length: {record_length}")
                logger.warning(f"Last {len(ebcdic_data) % record_length} bytes will be ignored")
            
            num_records = len(ebcdic_data) // record_length
            logger.info(f"Records to process: {num_records}")
            
            # Convert records
            converted_records = []
            self.ebcdic_converter.conversion_stats['total_records'] = num_records
            
            for i in range(num_records):
                try:
                    start_pos = i * record_length
                    end_pos = start_pos + record_length
                    record_data = ebcdic_data[start_pos:end_pos]
                    
                    converted_record = self.ebcdic_converter.convert_record(record_data, fields, sosi_config)
                    converted_record['_record_number'] = i + 1
                    converted_records.append(converted_record)
                    
                    self.ebcdic_converter.conversion_stats['successful_records'] += 1
                    
                    if (i + 1) % 100 == 0:
                        logger.info(f"Processed: {i + 1}/{num_records} records")
                        
                except Exception as e:
                    self.ebcdic_converter.conversion_stats['error_records'] += 1
                    self.ebcdic_converter.conversion_stats['conversion_errors'].append(
                        f"Record {i + 1} error: {e}"
                    )
                    logger.error(f"Record {i + 1} conversion failed: {e}")
                    continue
            
            # Create output directory if needed
            os.makedirs(os.path.dirname(output_file), exist_ok=True)
            
            # Save converted data based on output format
            if output_format.lower() == 'flat':
                self._write_flat_format(output_file, converted_records, fields, sosi_config)
            else:
                # Default JSON format
                output_data = {
                    'conversion_info': {
                        'timestamp': datetime.now().isoformat(),
                        'source_file': input_file,
                        'layout_file': layout_file,
                        'record_length': record_length,
                        'encoding': 'JAK',
                        'statistics': self.ebcdic_converter.conversion_stats
                    },
                    'records': converted_records
                }
                
                with open(output_file, 'w', encoding='utf-8') as f:
                    json.dump(output_data, f, indent=2, ensure_ascii=False)
            
            logger.info(f"Conversion completed: {output_file}")
            self._print_conversion_stats()
            
            # Register in catalog if dataset name provided
            if output_dataset_name:
                # Calculate correct record length for catalog
                catalog_record_length = sum(field['length'] for field in fields)
                
                # Determine target encoding from SOSI config
                target_encoding = "ascii"
                if sosi_config and sosi_config.get('japanese_encoding') == 'sjis':
                    target_encoding = "shift_jis"
                
                conversion_info = {
                    'source_encoding': 'JAK',
                    'target_encoding': target_encoding,
                    'SOURCE_FILE': input_file,
                    'LAYOUT_FILE': layout_file if not schema_file else schema_file,
                    'SCHEMA_FILE': schema_file,
                    'CONVERTED_RECORDS': self.ebcdic_converter.conversion_stats['successful_records'],
                    'CONVERSION_DATE': datetime.now().isoformat() + 'Z',
                    'record_length': catalog_record_length,
                    'SOSI_CONFIG': sosi_config
                }
                
                if self.catalog_manager.register_converted_dataset(output_dataset_name, conversion_info, volume, library):
                    logger.info(f"Dataset registered in catalog: {volume}.{library}.{output_dataset_name}")
                else:
                    logger.warning("Failed to register dataset in catalog")
            
            return True
            
        except Exception as e:
            logger.error(f"Conversion failed: {e}")
            return False
    
    def _write_flat_format(self, output_file: str, records: List[Dict], fields: List[Dict], sosi_config: Optional[Dict] = None):
        """Write records in FLAT format based on COBOL layout - Fixed Block without newlines"""
        try:
            # Calculate total record length from layout
            record_length = sum(field['length'] for field in fields)
            logger.info(f"Writing FLAT format with record length: {record_length} bytes")
            
            # Determine encoding from SOSI config
            target_encoding = 'shift_jis'
            if sosi_config and sosi_config.get('japanese_encoding') == 'utf-8':
                target_encoding = 'utf-8'
            
            logger.info(f"Using encoding: {target_encoding}")
            
            with open(output_file, 'wb') as f:  # Binary mode for Fixed Block
                for record in records:
                    record_bytes = bytearray()
                    
                    for field in fields:
                        field_name = field['name']
                        field_length = field['length']
                        field_type = field['type']
                        field_picture = field.get('picture', '')
                        
                        # Get field value, exclude internal fields
                        if field_name in record and field_name != '_record_number':
                            value = str(record[field_name])
                        else:
                            value = ""
                        
                        # Convert Unicode escape sequences to actual characters
                        if '\\u' in value:
                            try:
                                # Handle multiple Unicode escape sequences
                                import re
                                def decode_unicode_escapes(text):
                                    def replace_escape(match):
                                        try:
                                            return chr(int(match.group(1), 16))
                                        except:
                                            return match.group(0)  # Return original if conversion fails
                                    return re.sub(r'\\u([0-9a-fA-F]{4})', replace_escape, text)
                                
                                value = decode_unicode_escapes(value)
                            except:
                                pass  # Keep original value if decoding fails
                        
                        # Convert to bytes with proper encoding first, then format
                        try:
                            # For multi-byte encodings, we need to work with bytes, not characters
                            if field_type in ['COMP', 'COMP-3']:
                                # Numeric fields: right-aligned, zero-padded
                                formatted_value = value.zfill(field_length)[:field_length]
                                field_bytes = formatted_value.encode(target_encoding, errors='replace')
                            elif 'PIC 9' in field_picture.upper():
                                # Numeric DISPLAY fields: right-aligned, zero-padded
                                # Remove non-numeric characters first
                                numeric_value = ''.join(c for c in value if c.isdigit())
                                formatted_value = numeric_value.zfill(field_length)[:field_length]
                                field_bytes = formatted_value.encode(target_encoding, errors='replace')
                            else:
                                # Character fields: encode first, then pad/truncate at byte level
                                try:
                                    # Try encoding without replacement first to detect encoding issues
                                    value_bytes = value.encode(target_encoding, errors='strict')
                                except UnicodeEncodeError as e:
                                    # Log encoding issues for user awareness
                                    logger.warning(f"Field {field_name}: Characters cannot be encoded in {target_encoding} - {e}")
                                    logger.warning(f"Field {field_name}: Using replacement characters or consider UTF-8 encoding")
                                    # Fall back to replacement mode
                                    value_bytes = value.encode(target_encoding, errors='replace')
                                
                                if len(value_bytes) >= field_length:
                                    field_bytes = value_bytes[:field_length]
                                else:
                                    # Pad with space bytes
                                    field_bytes = value_bytes + b' ' * (field_length - len(value_bytes))
                            
                            # Ensure field is exactly field_length bytes
                            if len(field_bytes) > field_length:
                                field_bytes = field_bytes[:field_length]
                            elif len(field_bytes) < field_length:
                                field_bytes += b' ' * (field_length - len(field_bytes))
                            
                            record_bytes.extend(field_bytes)
                            
                        except UnicodeEncodeError:
                            # Fallback to ASCII if encoding fails
                            fallback_value = value.encode('ascii', errors='replace').decode('ascii')
                            formatted_fallback = fallback_value.ljust(field_length)[:field_length]
                            record_bytes.extend(formatted_fallback.encode('ascii'))
                    
                    # Ensure record is exactly record_length bytes
                    if len(record_bytes) < record_length:
                        record_bytes.extend(b' ' * (record_length - len(record_bytes)))
                    elif len(record_bytes) > record_length:
                        record_bytes = record_bytes[:record_length]
                    
                    f.write(record_bytes)  # No newline for Fixed Block
            
            logger.info(f"FLAT format (Fixed Block) written: {output_file}")
            logger.info(f"Total bytes written: {len(records) * record_length}")
            logger.info(f"Encoding used: {target_encoding}")
            
        except Exception as e:
            logger.error(f"Failed to write FLAT format: {e}")
            raise
    
    def _print_conversion_stats(self):
        """Print conversion statistics"""
        stats = self.ebcdic_converter.conversion_stats
        logger.info("\n=== Conversion Statistics ===")
        logger.info(f"Total records: {stats['total_records']}")
        logger.info(f"Successful records: {stats['successful_records']}")
        logger.info(f"Failed records: {stats['error_records']}")
        
        if stats['total_records'] > 0:
            success_rate = (stats['successful_records'] / stats['total_records']) * 100
            logger.info(f"Success rate: {success_rate:.2f}%")
        
        if stats['conversion_errors']:
            logger.info(f"\nConversion errors ({len(stats['conversion_errors'])}):")
            for error in stats['conversion_errors'][:5]:
                logger.info(f"  - {error}")
            if len(stats['conversion_errors']) > 5:
                logger.info(f"  ... and {len(stats['conversion_errors']) - 5} more")

def main():
    """Main function for command line usage"""
    import argparse
    
    parser = argparse.ArgumentParser(description='OpenASP EBCDIC Dataset Converter')
    parser.add_argument('input_file', help='Input EBCDIC file path')
    parser.add_argument('output_file', help='Output ASCII file path')
    parser.add_argument('layout_file', help='COBOL layout file path')
    parser.add_argument('--dataset-name', help='Dataset name for catalog registration')
    parser.add_argument('--format', choices=['json', 'flat'], default='json', 
                       help='Output format (default: json)')
    
    # SOSI options
    parser.add_argument('--so-code', default='0x0E', 
                       help='Shift Out code in hex format (default: 0x0E)')
    parser.add_argument('--si-code', default='0x0F', 
                       help='Shift In code in hex format (default: 0x0F)')
    parser.add_argument('--sosi-handling', choices=['SPACE', 'SOSI', 'REMOVE'], default='SPACE',
                       help='SOSI code handling: SPACE (replace with space), SOSI (keep codes), REMOVE (remove codes)')
    parser.add_argument('--japanese-encoding', choices=['utf-8', 'sjis'], default='utf-8',
                       help='Japanese character encoding: utf-8 (default), sjis')
    
    # Volume and library options
    parser.add_argument('--volume', default='DISK01',
                       help='Target volume name for catalog registration (default: DISK01)')
    parser.add_argument('--library', default='TESTLIB',
                       help='Target library name for catalog registration (default: TESTLIB)')
    
    # Schema option
    parser.add_argument('--schema', 
                       help='JSON schema file path (alternative to COBOL layout file)')
    
    args = parser.parse_args()
    
    # Parse SOSI configuration
    sosi_config = None
    if args.so_code or args.si_code or args.sosi_handling != 'SPACE' or args.japanese_encoding != 'utf-8':
        sosi_config = {
            'so_code': int(args.so_code, 16),
            'si_code': int(args.si_code, 16),
            'sosi_handling': args.sosi_handling,
            'japanese_encoding': args.japanese_encoding
        }
        print(f"SOSI Configuration: SO=0x{sosi_config['so_code']:02X}, SI=0x{sosi_config['si_code']:02X}, handling={sosi_config['sosi_handling']}, encoding={sosi_config['japanese_encoding']}")
    
    # Display configuration summary
    print(f"Volume: {args.volume}, Library: {args.library}")
    if args.schema:
        print(f"Using JSON Schema: {args.schema}")
    
    converter = OpenASPDatasetConverter()
    success = converter.convert_dataset(
        args.input_file, 
        args.output_file, 
        args.layout_file, 
        args.dataset_name, 
        args.format,
        sosi_config,
        args.volume,
        args.library,
        args.schema
    )
    
    if success:
        print(f"\nConversion completed successfully: {args.output_file}")
        if args.dataset_name:
            print(f"Dataset registered in catalog: {args.dataset_name}")
        sys.exit(0)
    else:
        print("\nConversion failed")
        sys.exit(1)

if __name__ == "__main__":
    main()