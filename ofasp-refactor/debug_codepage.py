#!/usr/bin/env python3
"""
コードページテーブルデバッグスクリプト
"""

import sys
from pathlib import Path

project_root = Path(__file__).parent
sys.path.insert(0, str(project_root / 'src' / 'utils'))

from ebcdic_batch_converter import EbcdicBatchConverter

def debug_codepage_table():
    converter = EbcdicBatchConverter()
    
    # JPコードページテーブルロード
    single_byte_table, double_byte_table = converter.load_codepage_table('JP', 'EBCDIC_TO_ASCII')
    
    print("=== ダブルバイトテーブルデバッグ ===")
    print(f"ダブルバイトテーブルサイズ: {len(double_byte_table)}")
    
    # 43AAマッピング確認
    test_value = 0x43AA
    print(f"\n43AAマッピングテスト:")
    print(f"  テスト値: 0x{test_value:04X} = {test_value}")
    
    if test_value in double_byte_table:
        mapped_value = double_byte_table[test_value]
        print(f"  [OK] マッピング済み: 0x{test_value:04X} -> 0x{mapped_value:04X}")
        
        # Shift-JIS変換テスト
        if mapped_value >= 0x8140:
            try:
                sjis_bytes = bytes([(mapped_value >> 8) & 0xFF, mapped_value & 0xFF])
                sjis_char = sjis_bytes.decode('shift_jis', errors='replace')
                print(f"  Shift-JIS: 0x{mapped_value:04X} -> '{sjis_char}' (U+{ord(sjis_char):04X})")
            except Exception as e:
                print(f"  Shift-JIS変換失敗: {e}")
    else:
        print(f"  [NG] マッピングされていません: 0x{test_value:04X}")
    
    # いくつかのダブルバイトマッピングサンプル出力
    print(f"\nダブルバイトマッピングサンプル (43A0-43AF):")
    for i in range(0x43A0, 0x43B0):
        if i in double_byte_table:
            mapped = double_byte_table[i]
            print(f"  0x{i:04X} -> 0x{mapped:04X}")
    
    # シングルバイトテーブルも確認
    print(f"\nシングルバイトテーブルサイズ: {len(single_byte_table)}")
    print(f"0x43 -> 0x{single_byte_table[0x43]:02X}")
    print(f"0xAA -> 0x{single_byte_table[0xAA]:02X}")

if __name__ == "__main__":
    debug_codepage_table()