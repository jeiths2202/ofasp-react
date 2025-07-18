#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Command line tool for EBCDIC file conversion
"""

import sys
import argparse
from pathlib import Path

# Add project root to Python path
sys.path.insert(0, str(Path(__file__).parent))

from src.converters.ebcdic_converter import converter
from src.constants.conversion import ConversionConstants


def main():
    parser = argparse.ArgumentParser(description='Convert EBCDIC file to ASCII')
    parser.add_argument('input_file', help='Input EBCDIC file path')
    parser.add_argument('-o', '--output', help='Output file path (default: stdout)')
    parser.add_argument('-e', '--encoding', default='JP', choices=['US', 'JP', 'JAK', 'KEIS', 'KR'],
                        help='Encoding type (default: JP)')
    parser.add_argument('-s', '--sosi', action='store_true', help='Enable SOSI processing')
    parser.add_argument('--sosi-handling', choices=['remove', 'keep', 'space'], default='remove',
                        help='SOSI handling mode (default: remove)')
    parser.add_argument('-r', '--rlen', type=int, default=80, help='Record length (default: 80)')
    
    args = parser.parse_args()
    
    # Check if input file exists
    input_path = Path(args.input_file)
    if not input_path.exists():
        print(f"Error: Input file not found: {args.input_file}", file=sys.stderr)
        sys.exit(1)
    
    # Read EBCDIC file
    with open(input_path, 'rb') as f:
        ebcdic_data = f.read()
    
    # Convert to hex string
    hex_data = ebcdic_data.hex().upper()
    
    # Process by record length
    converted_lines = []
    hex_line_length = args.rlen * 2
    
    for i in range(0, len(hex_data), hex_line_length):
        line_hex = hex_data[i:i+hex_line_length]
        if line_hex:
            # Convert line
            ascii_line = converter.convert_ebcdic_to_ascii(
                input_data=line_hex,
                encoding=args.encoding,
                sosi_flag=args.sosi,
                out_sosi_flag=(args.sosi_handling == 'keep'),
                rlen=args.rlen,
                sosi_handling=args.sosi_handling
            )
            converted_lines.append(ascii_line)
    
    # Join lines
    result = '\n'.join(converted_lines)
    
    # Output result
    if args.output:
        # Determine output encoding
        output_encoding = ConversionConstants.FILE_ENCODING_MAP.get(args.encoding, 'utf-8')
        try:
            with open(args.output, 'w', encoding=output_encoding) as f:
                f.write(result)
            print(f"Conversion completed. Output saved to: {args.output}")
        except Exception as e:
            print(f"Error writing output file: {e}", file=sys.stderr)
            sys.exit(1)
    else:
        # Print to stdout
        print(result)


if __name__ == '__main__':
    main()