#!/usr/bin/env python3
"""
Test script to analyze the real EBCDIC files and identify conversion issues
"""

import sys
from pathlib import Path

# Add project root to Python path
project_root = Path(__file__).parent
sys.path.insert(0, str(project_root / 'src' / 'utils'))

from ebcdic_batch_converter import EbcdicBatchConverter

def test_real_ebcdic_files():
    """Test conversion of real EBCDIC files"""
    
    converter = EbcdicBatchConverter()
    
    # Test data from the actual files
    test_cases = [
        {
            'name': 'CBACC484 - Line with SOSI',
            'data': '4040404040404040404040C4C9E2D7D3C1E8407D0E60FA4660FA8D61428660FA8D667C457F488D60F9805573657260F9802041757468656E7469636174696F6E272E2020202020202020202020202020',
            'encoding': 'JP'
        },
        {
            'name': 'CBACC591 - Line with SOSI and mixed content',
            'data': '4040404040404040404040C4C9E2D7D3C1E8407D0E614248A060FA4960FA8D6544866A64FC5D9D80804572726F7260F9802041757468656E7469636174696F6E272E2020202020202020202020202020',
            'encoding': 'JP'
        }
    ]
    
    for test_case in test_cases:
        print(f"\n{'='*60}")
        print(f"Testing: {test_case['name']}")
        print(f"{'='*60}")
        
        result = converter.EBCDIC_TO_ASCII(
            input_data=test_case['data'],
            output_buffer=None,
            encoding=test_case['encoding'],
            sosi_flag=True,
            out_sosi_flag=False,
            rlen=80,
            layout=None
        )
        
        print(f"Result: \"{result}\"")
        
        # Show hex representation of result
        hex_result = ' '.join(f'{ord(c):02X}' for c in result)
        print(f"Result hex: {hex_result}")

if __name__ == "__main__":
    test_real_ebcdic_files()