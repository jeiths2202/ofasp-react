#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Production EBCDIC Converter - NO HARDCODING, NO TEST CODE
Clean conversion service following CODING_RULES.md
"""

import sys
from pathlib import Path
from typing import Dict, Tuple, Optional, Union, List
from functools import lru_cache

# Add project root to Python path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from config.config import config
from src.constants.conversion import ConversionConstants, ValidationRules
from src.utils.logger import conversion_logger


class EbcdicConverter:
    """Production EBCDIC converter with configurable settings"""
    
    def __init__(self):
        """Initialize converter with configuration"""
        self.config = config
        self.constants = ConversionConstants()
        self.logger = conversion_logger
        self._mapping_cache: Dict[str, Tuple[Dict[int, int], Dict[int, int]]] = {}
        
        self.logger.info(f"EbcdicConverter initialized with codepage path: {self.config.codepage_config['base_path']}")
    
    @lru_cache(maxsize=None)
    def _load_codepage_table(self, encoding: str, direction: str) -> Tuple[Dict[int, int], Dict[int, int]]:
        """
        Load codepage table with caching
        
        Args:
            encoding: Encoding type (US, JP, JAK, KEIS, KR)
            direction: Conversion direction (EBCDIC_TO_ASCII, ASCII_TO_EBCDIC)
            
        Returns:
            Tuple of (single_byte_table, double_byte_table)
        """
        cache_key = f"{encoding}_{direction}"
        
        if cache_key in self._mapping_cache:
            return self._mapping_cache[cache_key]
        
        # Validate encoding
        if not ValidationRules.is_valid_encoding(encoding):
            raise ValueError(f"Unsupported encoding: {encoding}")
        
        # Get file path from configuration
        file_path = self.config.get_codepage_file_path(encoding, direction)
        
        if not file_path.exists():
            raise FileNotFoundError(f"Codepage file not found: {file_path}")
        
        self.logger.info(f"Loading codepage table: {file_path}")
        
        single_byte_table = {}
        double_byte_table = {}
        
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                lines = f.readlines()
            
            current_section = None
            single_byte_count = 0
            double_byte_count = 0
            
            for line_num, line in enumerate(lines, 1):
                line = line.strip()
                
                # Section headers
                if line == '[Single byte mapping table]':
                    current_section = 'single'
                    continue
                elif line == '[Double byte mapping table]':
                    current_section = 'double'
                    continue
                
                # Skip empty lines
                if not line:
                    continue
                
                # Parse mapping lines
                if ' - ' in line:
                    try:
                        from_hex, to_hex = line.split(' - ')
                        from_value = int(from_hex, 16)
                        to_value = int(to_hex, 16)
                        
                        if current_section == 'single':
                            single_byte_table[from_value] = to_value
                            single_byte_count += 1
                        elif current_section == 'double':
                            # Skip zero mappings (unmapped)
                            if to_value != 0:
                                double_byte_table[from_value] = to_value
                                double_byte_count += 1
                    except ValueError as e:
                        self.logger.warning(f"Invalid mapping line {line_num}: {line} - {e}")
                        continue
            
            self.logger.info(f"Loaded {single_byte_count} single-byte and {double_byte_count} double-byte mappings")
            
            # Cache the result
            self._mapping_cache[cache_key] = (single_byte_table, double_byte_table)
            
            return single_byte_table, double_byte_table
            
        except Exception as e:
            self.logger.error(f"Failed to load codepage table {file_path}: {e}")
            raise RuntimeError(f"Failed to load codepage table: {e}")
    
    def convert_ebcdic_to_ascii(self, 
                               input_data: Union[str, bytes],
                               encoding: str = None,
                               sosi_flag: bool = False,
                               out_sosi_flag: bool = False,
                               rlen: int = None,
                               layout: Optional[str] = None,
                               sosi_handling: str = 'remove') -> str:
        """
        Convert EBCDIC to ASCII
        
        Args:
            input_data: Input data (hex string or bytes)
            encoding: Encoding type
            sosi_flag: Enable SOSI processing
            out_sosi_flag: Keep SOSI codes in output
            rlen: Record length
            layout: Layout information (unused)
            sosi_handling: SOSI handling mode ('remove', 'keep', 'space')
            
        Returns:
            Converted ASCII string
        """
        # Use configuration defaults if not provided
        if encoding is None:
            encoding = self.config.conversion_config['default_encoding']
        if rlen is None:
            rlen = self.config.conversion_config['default_record_length']
        
        # Validate parameters
        if not ValidationRules.is_valid_encoding(encoding):
            raise ValueError(f"Invalid encoding: {encoding}")
        if not ValidationRules.is_valid_record_length(rlen):
            raise ValueError(f"Invalid record length: {rlen}")
        
        self.logger.debug(f"Converting EBCDIC to ASCII: encoding={encoding}, sosi={sosi_flag}, rlen={rlen}")
        
        # Load codepage table
        single_byte_table, double_byte_table = self._load_codepage_table(encoding, ConversionConstants.EBCDIC_TO_ASCII)
        
        # Convert input to bytes
        if isinstance(input_data, str):
            if not ValidationRules.is_valid_hex_string(input_data):
                raise ValueError("Invalid hex string format")
            
            clean_input = input_data.replace(' ', '').replace('\n', '')
            if len(clean_input) % 2 != 0:
                clean_input += '0'  # Pad odd length
            bytes_data = bytes.fromhex(clean_input)
        else:
            bytes_data = input_data
        
        # Apply record length limit
        if rlen > 0 and len(bytes_data) > rlen:
            bytes_data = bytes_data[:rlen]
        
        # Check input size limit
        max_size = self.config.conversion_config['max_input_size']
        if len(bytes_data) > max_size:
            raise ValueError(f"Input size exceeds maximum allowed: {len(bytes_data)} > {max_size}")
        
        result = []
        i = 0
        is_double_byte_mode = False
        
        while i < len(bytes_data):
            byte = bytes_data[i]
            
            if sosi_flag:
                # Handle SOSI codes
                if byte == ConversionConstants.SOSI_SO:
                    is_double_byte_mode = True
                    # Handle SOSI based on sosi_handling parameter
                    if sosi_handling == 'keep' or out_sosi_flag:
                        result.append(chr(byte))
                    elif sosi_handling == 'space':
                        result.append(' ')  # Replace with space
                    # else: 'remove' - don't add anything
                    i += 1
                    continue
                    
                elif byte == ConversionConstants.SOSI_SI:
                    is_double_byte_mode = False
                    # Handle SOSI based on sosi_handling parameter
                    if sosi_handling == 'keep' or out_sosi_flag:
                        result.append(chr(byte))
                    elif sosi_handling == 'space':
                        result.append(' ')  # Replace with space
                    # else: 'remove' - don't add anything
                    i += 1
                    continue
                
                # Handle double-byte mode
                if is_double_byte_mode:
                    if i + 1 < len(bytes_data):
                        next_byte = bytes_data[i + 1]
                        
                        # Check if current byte is SI
                        if byte == ConversionConstants.SOSI_SI:
                            is_double_byte_mode = False
                            # Handle SOSI based on sosi_handling parameter
                            if sosi_handling == 'keep' or out_sosi_flag:
                                result.append(chr(byte))
                            elif sosi_handling == 'space':
                                result.append(' ')  # Replace with space
                            # else: 'remove' - don't add anything
                            i += 1
                            continue
                        
                        # Check if next byte is SI
                        if next_byte == ConversionConstants.SOSI_SI:
                            # Odd byte - process as single byte
                            ascii_value = single_byte_table.get(byte, ConversionConstants.DEFAULT_SPACE_CHAR)
                            result.append(chr(ascii_value))
                            i += 1
                            continue
                        
                        # Process double-byte character
                        double_byte_value = (byte << 8) | next_byte
                        
                        if double_byte_value in double_byte_table:
                            mapped_value = double_byte_table[double_byte_value]
                            
                            if mapped_value == ConversionConstants.SHIFT_JIS_FULL_WIDTH_SPACE:
                                result.append(' ')
                            elif mapped_value <= 0xFF:
                                result.append(chr(mapped_value))
                            else:
                                # Handle Shift-JIS decoding for Japanese
                                if encoding == 'JP' and mapped_value >= ConversionConstants.SHIFT_JIS_DOUBLE_BYTE_MIN:
                                    try:
                                        sjis_bytes = bytes([(mapped_value >> 8) & 0xFF, mapped_value & 0xFF])
                                        sjis_char = sjis_bytes.decode('shift_jis', errors='replace')
                                        result.append(sjis_char)
                                    except UnicodeDecodeError:
                                        result.append('?')
                                else:
                                    result.append(' ')
                        else:
                            result.append(' ')
                        
                        i += 2
                        continue
                    else:
                        # Last byte in double-byte mode
                        if byte == ConversionConstants.SOSI_SI:
                            is_double_byte_mode = False
                            # Handle SOSI based on sosi_handling parameter
                            if sosi_handling == 'keep' or out_sosi_flag:
                                result.append(chr(byte))
                            elif sosi_handling == 'space':
                                result.append(' ')  # Replace with space
                            # else: 'remove' - don't add anything
                            i += 1
                            continue
                        
                        ascii_value = single_byte_table.get(byte, ConversionConstants.DEFAULT_SPACE_CHAR)
                        result.append(chr(ascii_value))
                        i += 1
                        continue
            
            # Single-byte mode or SOSI disabled
            ascii_value = single_byte_table.get(byte, ConversionConstants.DEFAULT_SPACE_CHAR)
            result.append(chr(ascii_value))
            i += 1
        
        ascii_result = ''.join(result)
        self.logger.debug(f"Conversion completed: {len(ascii_result)} characters")
        
        return ascii_result
    
    def convert_ascii_to_ebcdic(self,
                               input_data: Union[str, bytes],
                               encoding: str = None,
                               sosi_flag: bool = False,
                               out_sosi_flag: bool = False,
                               rlen: int = None,
                               layout: Optional[str] = None) -> str:
        """
        Convert ASCII to EBCDIC
        
        Args:
            input_data: Input ASCII string
            encoding: Encoding type
            sosi_flag: Enable SOSI processing
            out_sosi_flag: Keep SOSI codes in output
            rlen: Record length
            layout: Layout information (unused)
            
        Returns:
            Converted EBCDIC hex string
        """
        # Use configuration defaults if not provided
        if encoding is None:
            encoding = self.config.conversion_config['default_encoding']
        if rlen is None:
            rlen = self.config.conversion_config['default_record_length']
        
        # Validate parameters
        if not ValidationRules.is_valid_encoding(encoding):
            raise ValueError(f"Invalid encoding: {encoding}")
        if not ValidationRules.is_valid_record_length(rlen):
            raise ValueError(f"Invalid record length: {rlen}")
        
        self.logger.debug(f"Converting ASCII to EBCDIC: encoding={encoding}, sosi={sosi_flag}, rlen={rlen}")
        
        # Load codepage table
        single_byte_table, double_byte_table = self._load_codepage_table(encoding, ConversionConstants.ASCII_TO_EBCDIC)
        
        # Convert input to string
        if isinstance(input_data, bytes):
            input_data = input_data.decode('utf-8', errors='replace')
        
        # Apply record length limit
        if rlen > 0 and len(input_data) > rlen:
            input_data = input_data[:rlen]
        
        # Check input size limit
        max_size = self.config.conversion_config['max_input_size']
        if len(input_data) > max_size:
            raise ValueError(f"Input size exceeds maximum allowed: {len(input_data)} > {max_size}")
        
        result = []
        
        for char in input_data:
            ascii_value = ord(char)
            
            if ascii_value in single_byte_table:
                ebcdic_value = single_byte_table[ascii_value]
                result.append(f'{ebcdic_value:02X}')
            else:
                # Use EBCDIC space for unmapped characters
                result.append(f'{ConversionConstants.EBCDIC_SPACE_CHAR:02X}')
        
        ebcdic_result = ''.join(result)
        self.logger.debug(f"Conversion completed: {len(ebcdic_result)} hex characters")
        
        return ebcdic_result
    
    def convert_file(self, 
                    input_path: str, 
                    output_path: str,
                    encoding: str = None,
                    sosi_flag: bool = True,
                    out_sosi_flag: bool = False,
                    rlen: int = None,
                    direction: str = 'EBCDIC_TO_ASCII') -> str:
        """
        Convert file from EBCDIC to ASCII or vice versa
        
        Args:
            input_path: Input file path
            output_path: Output file path
            encoding: Encoding type
            sosi_flag: Enable SOSI processing
            out_sosi_flag: Keep SOSI codes in output
            rlen: Record length
            direction: Conversion direction
            
        Returns:
            Converted content
        """
        # Use configuration defaults if not provided
        if encoding is None:
            encoding = self.config.conversion_config['default_encoding']
        if rlen is None:
            rlen = self.config.conversion_config['default_record_length']
        
        input_file = Path(input_path)
        output_file = Path(output_path)
        
        if not input_file.exists():
            raise FileNotFoundError(f"Input file not found: {input_path}")
        
        # Create output directory if it doesn't exist
        output_file.parent.mkdir(parents=True, exist_ok=True)
        
        self.logger.info(f"Converting file: {input_path} -> {output_path}")
        
        if direction == ConversionConstants.EBCDIC_TO_ASCII:
            # Read EBCDIC file as binary
            with open(input_file, 'rb') as f:
                ebcdic_data = f.read()
            
            # Convert to hex string
            hex_data = ebcdic_data.hex().upper()
            
            # Process in record-length chunks
            converted_lines = []
            hex_line_length = rlen * 2  # rlen bytes = rlen*2 hex chars
            
            for i in range(0, len(hex_data), hex_line_length):
                line_hex = hex_data[i:i+hex_line_length]
                if line_hex:
                    ascii_line = self.convert_ebcdic_to_ascii(
                        input_data=line_hex,
                        encoding=encoding,
                        sosi_flag=sosi_flag,
                        out_sosi_flag=out_sosi_flag,
                        rlen=rlen
                    )
                    converted_lines.append(ascii_line)
            
            # Join lines and save
            full_content = '\n'.join(converted_lines)
            
            # Determine output encoding
            output_encoding = ConversionConstants.FILE_ENCODING_MAP.get(encoding, 'utf-8')
            
            try:
                with open(output_file, 'w', encoding=output_encoding) as f:
                    f.write(full_content)
                self.logger.info(f"File saved with {output_encoding} encoding")
            except UnicodeEncodeError:
                # Fallback to UTF-8
                with open(output_file, 'w', encoding='utf-8') as f:
                    f.write(full_content)
                self.logger.warning(f"Fallback to UTF-8 encoding for {output_path}")
        
        else:
            # ASCII to EBCDIC conversion
            with open(input_file, 'r', encoding='utf-8') as f:
                ascii_content = f.read()
            
            ebcdic_hex = self.convert_ascii_to_ebcdic(
                input_data=ascii_content,
                encoding=encoding,
                sosi_flag=sosi_flag,
                out_sosi_flag=out_sosi_flag,
                rlen=rlen
            )
            
            # Convert hex string to bytes and save
            ebcdic_bytes = bytes.fromhex(ebcdic_hex)
            with open(output_file, 'wb') as f:
                f.write(ebcdic_bytes)
            
            full_content = ebcdic_hex
        
        self.logger.info(f"File conversion completed: {output_path}")
        return full_content


# Global converter instance
converter = EbcdicConverter()

# Export for easy import
__all__ = ['EbcdicConverter', 'converter']