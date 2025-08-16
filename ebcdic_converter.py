#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
EBCDIC Dataset Converter Tool
EBCDIC 데이터셋을 레이아웃 기반으로 ASCII/SJIS로 변환하는 도구

Usage:
    python ebcdic_converter.py --input /data/assets/ebcdic/DEMO.SAM.ebc \
                              --output /tmp/a.out \
                              --layout /home/aspuser/app/volume/DISK01/LAYOUT/SAM001.LAYOUT \
                              --record-length 80 \
                              --encoding JAK \
                              --sosi-so 0x28 \
                              --sosi-si 0x29 \
                              --convert-sosi-to-space
"""

import os
import sys
import argparse
import struct
import logging
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass
from pathlib import Path
import re

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('/tmp/ebcdic_converter.log'),
        logging.StreamHandler(sys.stdout)
    ]
)
logger = logging.getLogger(__name__)

@dataclass
class LayoutField:
    """COBOL レイアウトフィールド定義"""
    name: str
    level: str
    type: str  # 'PIC' | 'COMP' | 'COMP-3'
    picture: str
    length: int
    position: int
    description: str = ""

@dataclass
class ConversionConfig:
    """変換設定"""
    input_file: str
    output_file: str
    layout_file: str
    record_length: int
    encoding: str
    sosi_so: int = 0x28
    sosi_si: int = 0x29
    convert_sosi_to_space: bool = True

class EBCDICConverter:
    """EBCDIC データセット変換ツール"""
    
    # EBCDIC to ASCII conversion tables
    EBCDIC_TO_ASCII_JAK = {
        # JAK (Japanese EBCDIC) conversion table
        # Basic ASCII characters (0x00-0x7F)
        **{i: i for i in range(0x80)},
        
        # Common EBCDIC mappings
        0x40: 0x20,  # Space
        0x4B: 0x2E,  # Period
        0x4C: 0x3C,  # Less than
        0x4D: 0x28,  # Left parenthesis
        0x4E: 0x2B,  # Plus
        0x4F: 0x7C,  # Vertical bar
        0x50: 0x26,  # Ampersand
        0x5A: 0x21,  # Exclamation mark
        0x5B: 0x24,  # Dollar sign
        0x5C: 0x2A,  # Asterisk
        0x5D: 0x29,  # Right parenthesis
        0x5E: 0x3B,  # Semicolon
        0x60: 0x2D,  # Hyphen/minus
        0x61: 0x2F,  # Slash
        0x6B: 0x2C,  # Comma
        0x6C: 0x25,  # Percent
        0x6D: 0x5F,  # Underscore
        0x6E: 0x3E,  # Greater than
        0x6F: 0x3F,  # Question mark
        0x7A: 0x3A,  # Colon
        0x7B: 0x23,  # Number sign
        0x7C: 0x40,  # At sign
        0x7D: 0x27,  # Apostrophe
        0x7E: 0x3D,  # Equal sign
        0x7F: 0x22,  # Quotation mark
        
        # Letters A-I
        0x81: 0x61, 0x82: 0x62, 0x83: 0x63, 0x84: 0x64, 0x85: 0x65,
        0x86: 0x66, 0x87: 0x67, 0x88: 0x68, 0x89: 0x69,
        
        # Letters J-R
        0x91: 0x6A, 0x92: 0x6B, 0x93: 0x6C, 0x94: 0x6D, 0x95: 0x6E,
        0x96: 0x6F, 0x97: 0x70, 0x98: 0x71, 0x99: 0x72,
        
        # Letters S-Z
        0xA2: 0x73, 0xA3: 0x74, 0xA4: 0x75, 0xA5: 0x76, 0xA6: 0x77,
        0xA7: 0x78, 0xA8: 0x79, 0xA9: 0x7A,
        
        # Uppercase letters A-I
        0xC1: 0x41, 0xC2: 0x42, 0xC3: 0x43, 0xC4: 0x44, 0xC5: 0x45,
        0xC6: 0x46, 0xC7: 0x47, 0xC8: 0x48, 0xC9: 0x49,
        
        # Uppercase letters J-R
        0xD1: 0x4A, 0xD2: 0x4B, 0xD3: 0x4C, 0xD4: 0x4D, 0xD5: 0x4E,
        0xD6: 0x4F, 0xD7: 0x50, 0xD8: 0x51, 0xD9: 0x52,
        
        # Uppercase letters S-Z
        0xE2: 0x53, 0xE3: 0x54, 0xE4: 0x55, 0xE5: 0x56, 0xE6: 0x57,
        0xE7: 0x58, 0xE8: 0x59, 0xE9: 0x5A,
        
        # Numbers 0-9
        0xF0: 0x30, 0xF1: 0x31, 0xF2: 0x32, 0xF3: 0x33, 0xF4: 0x34,
        0xF5: 0x35, 0xF6: 0x36, 0xF7: 0x37, 0xF8: 0x38, 0xF9: 0x39,
    }
    
    def __init__(self, config: ConversionConfig):
        self.config = config
        self.layout_fields: List[LayoutField] = []
        
    def parse_layout_file(self) -> List[LayoutField]:
        """COBOL レイアウトファイルを解析してフィールド定義を抽出"""
        logger.info(f"レイアウトファイルを解析中: {self.config.layout_file}")
        
        if not os.path.exists(self.config.layout_file):
            raise FileNotFoundError(f"レイアウトファイルが見つかりません: {self.config.layout_file}")
        
        fields = []
        current_position = 1
        
        try:
            # Try SJIS encoding first for Japanese COBOL layouts
            with open(self.config.layout_file, 'r', encoding='shift_jis') as f:
                content = f.read()
        except UnicodeDecodeError:
            # Fallback to UTF-8
            with open(self.config.layout_file, 'r', encoding='utf-8') as f:
                content = f.read()
        
        lines = content.split('\n')
        
        for line_num, line in enumerate(lines, 1):
            # Skip comments and empty lines
            if line.strip().startswith('*') or line.strip() == '':
                continue
            
            # Match COBOL field definition pattern
            # Example: "03 EMPLOYEE-ID    PIC 9(5)."
            field_match = re.match(
                r'^\s*(\d+)\s+([A-Z0-9_-]+)\s+PIC\s+([X9SNVZ()\-]+)\.?(?:\s+(COMP(-3)?))?\s*(.*)$',
                line,
                re.IGNORECASE
            )
            
            if field_match:
                level, name, picture, comp, comp3, description = field_match.groups()
                
                # Skip group level items (01, 02 levels) - only process elementary items
                level_num = int(level)
                if level_num <= 2:
                    continue
                
                # Calculate field length from PIC clause
                length = self._calculate_field_length(picture, comp is not None, comp3 is not None)
                
                # Determine field type
                field_type = 'PIC'
                if comp3 or 'COMP-3' in line.upper():
                    field_type = 'COMP-3'
                    # COMP-3 fields use (length + 1) / 2 bytes
                    length = (length + 1) // 2
                elif comp or 'COMP ' in line.upper():
                    field_type = 'COMP'
                    # COMP fields are typically 2, 4, or 8 bytes
                    if length <= 4:
                        length = 2
                    elif length <= 9:
                        length = 4
                    else:
                        length = 8
                
                field = LayoutField(
                    name=name,
                    level=level,
                    type=field_type,
                    picture=picture,
                    length=length,
                    position=current_position,
                    description=description.strip() if description else line.strip()
                )
                
                fields.append(field)
                current_position += length
                
                logger.debug(f"フィールド解析: {field.name} ({field.type}) 位置={field.position} 長さ={field.length}")
        
        if not fields:
            raise ValueError("レイアウトファイルからフィールド定義を解析できませんでした")
        
        logger.info(f"解析完了: {len(fields)}個のフィールドが見つかりました")
        self.layout_fields = fields
        return fields
    
    def _calculate_field_length(self, picture: str, is_comp: bool, is_comp3: bool) -> int:
        """PIC句からフィールド長を計算"""
        picture = picture.upper().strip()
        
        # Handle parentheses notation: 9(5), X(20), etc.
        if '(' in picture and ')' in picture:
            # Extract length from parentheses: PIC 9(5) -> 5
            length_match = re.search(r'\((\d+)\)', picture)
            if length_match:
                return int(length_match.group(1))
        
        # Handle repeated characters: 999, XXX, etc.
        # Count consecutive occurrences of the same character
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
        
        return total_length if total_length > 0 else 1
    
    def convert_ebcdic_byte(self, byte_val: int) -> int:
        """単一バイトのEBCDIC→ASCII変換"""
        return self.EBCDIC_TO_ASCII_JAK.get(byte_val, byte_val)
    
    def process_sosi_codes(self, data: bytes) -> bytes:
        """SOSI コードを処理"""
        if not self.config.convert_sosi_to_space:
            return data
        
        # Convert SOSI codes to spaces
        processed = bytearray(data)
        for i in range(len(processed)):
            if processed[i] == self.config.sosi_so or processed[i] == self.config.sosi_si:
                processed[i] = 0x20  # Convert to space
        
        return bytes(processed)
    
    def convert_field_data(self, field: LayoutField, field_data: bytes) -> Tuple[bytes, bool]:
        """フィールドデータを変換"""
        if field.type in ['COMP', 'COMP-3']:
            # Binary fields: preserve as-is
            logger.debug(f"バイナリフィールド {field.name}: {field_data.hex()}")
            return field_data, True
        
        # PIC fields: convert EBCDIC to ASCII
        converted = bytearray()
        for byte_val in field_data:
            converted_byte = self.convert_ebcdic_byte(byte_val)
            converted.append(converted_byte)
        
        # Process SOSI codes
        converted_data = self.process_sosi_codes(bytes(converted))
        
        logger.debug(f"フィールド {field.name} 変換: {field_data.hex()} -> {converted_data.hex()}")
        return converted_data, False
    
    def convert_dataset(self) -> None:
        """データセット変換メイン処理"""
        logger.info("=== EBCDIC データセット変換開始 ===")
        logger.info(f"入力ファイル: {self.config.input_file}")
        logger.info(f"出力ファイル: {self.config.output_file}")
        logger.info(f"レイアウト: {self.config.layout_file}")
        logger.info(f"レコード長: {self.config.record_length}")
        logger.info(f"エンコーディング: {self.config.encoding}")
        logger.info(f"SOSI SO: 0x{self.config.sosi_so:02X}, SI: 0x{self.config.sosi_si:02X}")
        
        # Parse layout
        fields = self.parse_layout_file()
        total_field_length = sum(field.length for field in fields)
        
        logger.info(f"レイアウト解析完了: {len(fields)}フィールド, 合計長={total_field_length}バイト")
        
        if total_field_length > self.config.record_length:
            logger.warning(f"フィールド合計長({total_field_length}) > レコード長({self.config.record_length})")
        
        # Check input file
        if not os.path.exists(self.config.input_file):
            raise FileNotFoundError(f"入力ファイルが見つかりません: {self.config.input_file}")
        
        file_size = os.path.getsize(self.config.input_file)
        record_count = file_size // self.config.record_length
        
        logger.info(f"入力ファイルサイズ: {file_size} バイト")
        logger.info(f"予想レコード数: {record_count}")
        
        # Create output directory if needed
        os.makedirs(os.path.dirname(self.config.output_file), exist_ok=True)
        
        # Process conversion
        with open(self.config.input_file, 'rb') as infile, \
             open(self.config.output_file, 'wb') as outfile:
            
            records_processed = 0
            
            for record_index in range(record_count):
                # Read one record
                record_data = infile.read(self.config.record_length)
                if len(record_data) < self.config.record_length:
                    logger.warning(f"レコード {record_index + 1}: 不完全なデータ ({len(record_data)} < {self.config.record_length})")
                    break
                
                # Convert each field
                converted_record = bytearray()
                
                for field in fields:
                    field_start = field.position - 1  # Convert to 0-based index
                    field_end = field_start + field.length
                    
                    if field_end > len(record_data):
                        logger.warning(f"フィールド {field.name} がレコード境界を超えています")
                        field_data = record_data[field_start:] + b'\x00' * (field.length - (len(record_data) - field_start))
                    else:
                        field_data = record_data[field_start:field_end]
                    
                    converted_data, is_binary = self.convert_field_data(field, field_data)
                    converted_record.extend(converted_data)
                
                # Pad record to original length if needed
                while len(converted_record) < self.config.record_length:
                    converted_record.append(0x20)  # Space padding
                
                # Truncate if too long
                if len(converted_record) > self.config.record_length:
                    converted_record = converted_record[:self.config.record_length]
                
                outfile.write(converted_record)
                records_processed += 1
                
                if records_processed % 1000 == 0:
                    logger.info(f"処理済みレコード数: {records_processed}")
        
        logger.info(f"=== 変換完了 ===")
        logger.info(f"処理レコード数: {records_processed}")
        logger.info(f"出力ファイル: {self.config.output_file}")
        logger.info(f"出力ファイルサイズ: {os.path.getsize(self.config.output_file)} バイト")

def create_sample_layout():
    """サンプルレイアウトファイルを作成 (テスト用)"""
    sample_layout = """      * SAM001 Layout Definition
      * Record Format: Fixed Block (FB)
      * Record Length: 80 bytes
      * 
      * Field Definitions:
      01  EMPLOYEE-RECORD.
          03  EMPLOYEE-ID     PIC 9(5).
          03  EMPLOYEE-NAME   PIC X(20).
          03  DEPARTMENT      PIC X(10).
          03  HIRE-DATE       PIC X(8).
          03  STATUS          PIC X(1).
          03  EMAIL           PIC X(15).
          03  RESERVED        PIC X(21).
"""
    
    layout_dir = "/home/aspuser/app/volume/DISK01/LAYOUT/"
    os.makedirs(layout_dir, exist_ok=True)
    
    with open(f"{layout_dir}/SAM001.LAYOUT", 'w', encoding='utf-8') as f:
        f.write(sample_layout)
    
    logger.info(f"サンプルレイアウトファイル作成: {layout_dir}/SAM001.LAYOUT")

def create_sample_ebcdic_data():
    """サンプルEBCDICデータを作成 (テスト用)"""
    # Create sample EBCDIC data
    ebcdic_dir = "/data/assets/ebcdic/"
    os.makedirs(ebcdic_dir, exist_ok=True)
    
    # Sample record in EBCDIC format
    sample_records = [
        # Record 1: Employee ID 00001, Name "TARO TANAKA", etc.
        b'\xF0\xF0\xF0\xF0\xF1' +  # Employee ID: 00001
        b'TARO TANAKA        ' +  # Employee Name (20 bytes)
        b'SALES     ' +           # Department (10 bytes)
        b'20240401' +             # Hire Date (8 bytes)
        b'A' +                    # Status (1 byte)
        b'taro@example.com ' +    # Email (15 bytes)
        b' ' * 21,                # Reserved (21 bytes)
        
        # Record 2: Employee ID 00002, Name "HANAKO SATO", etc.
        b'\xF0\xF0\xF0\xF0\xF2' +  # Employee ID: 00002
        b'HANAKO SATO       ' +  # Employee Name (20 bytes)
        b'ADMIN     ' +           # Department (10 bytes)
        b'20230315' +             # Hire Date (8 bytes)
        b'A' +                    # Status (1 byte)
        b'hanako@example.co' +    # Email (15 bytes)
        b' ' * 21,                # Reserved (21 bytes)
    ]
    
    with open(f"{ebcdic_dir}/DEMO.SAM.ebc", 'wb') as f:
        for record in sample_records:
            # Ensure each record is exactly 80 bytes
            if len(record) < 80:
                record += b' ' * (80 - len(record))
            elif len(record) > 80:
                record = record[:80]
            f.write(record)
    
    logger.info(f"サンプルEBCDICデータ作成: {ebcdic_dir}/DEMO.SAM.ebc")

def main():
    """メイン処理"""
    parser = argparse.ArgumentParser(description='EBCDIC Dataset Converter')
    parser.add_argument('--input', help='Input EBCDIC file path')
    parser.add_argument('--output', help='Output converted file path')
    parser.add_argument('--layout', help='COBOL layout/copybook file path')
    parser.add_argument('--record-length', type=int, help='Record length in bytes')
    parser.add_argument('--encoding', default='JAK', help='EBCDIC encoding type (default: JAK)')
    parser.add_argument('--sosi-so', default='0x28', help='SOSI Shift-Out code (default: 0x28)')
    parser.add_argument('--sosi-si', default='0x29', help='SOSI Shift-In code (default: 0x29)')
    parser.add_argument('--convert-sosi-to-space', action='store_true', default=True,
                       help='Convert SOSI codes to spaces (default: True)')
    parser.add_argument('--create-sample', action='store_true',
                       help='Create sample layout and EBCDIC data files for testing')
    parser.add_argument('--verbose', '-v', action='store_true', help='Verbose logging')
    
    args = parser.parse_args()
    
    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)
    
    # Create sample data if requested
    if args.create_sample:
        create_sample_layout()
        create_sample_ebcdic_data()
        logger.info("サンプルファイル作成完了")
        return 0
    
    # Check required arguments for conversion
    if not all([args.input, args.output, args.layout, args.record_length]):
        parser.error("conversion requires --input, --output, --layout, and --record-length")
    
    
    # Parse SOSI codes
    sosi_so = int(args.sosi_so, 16) if args.sosi_so.startswith('0x') else int(args.sosi_so)
    sosi_si = int(args.sosi_si, 16) if args.sosi_si.startswith('0x') else int(args.sosi_si)
    
    # Create configuration
    config = ConversionConfig(
        input_file=args.input,
        output_file=args.output,
        layout_file=args.layout,
        record_length=args.record_length,
        encoding=args.encoding,
        sosi_so=sosi_so,
        sosi_si=sosi_si,
        convert_sosi_to_space=args.convert_sosi_to_space
    )
    
    try:
        # Create converter and run conversion
        converter = EBCDICConverter(config)
        converter.convert_dataset()
        
        logger.info("変換が正常に完了しました")
        return 0
        
    except Exception as e:
        logger.error(f"変換エラー: {e}")
        return 1

if __name__ == "__main__":
    sys.exit(main())