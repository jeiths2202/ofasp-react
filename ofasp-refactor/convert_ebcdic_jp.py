#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
EBCDIC to ASCII/Japanese 変換ツール
日本語エンコーディング(JP)の場合、自動的にShift-JISで保存
"""

import sys
import os
from pathlib import Path

# プロジェクトルートディレクトリ追加
project_root = Path(__file__).parent
sys.path.insert(0, str(project_root / 'src' / 'utils'))

from ebcdic_batch_converter import EbcdicBatchConverter

def main():
    """EBCDICファイル変換メイン関数"""
    
    if len(sys.argv) < 3:
        print("Usage: python3 convert_ebcdic_jp.py <input_ebcdic_file> <output_file> [encoding]")
        print("  encoding: US, JP, KR, JAK, KEIS (default: JP)")
        print("\nExample:")
        print("  python3 convert_ebcdic_jp.py /data/assets/DEMO/ebcdic/CBACC484 output.txt JP")
        sys.exit(1)
    
    input_file = sys.argv[1]
    output_file = sys.argv[2]
    encoding = sys.argv[3] if len(sys.argv) > 3 else 'JP'
    
    # 入力ファイル存在確認
    if not os.path.exists(input_file):
        print(f"エラー: 入力ファイルが見つかりません: {input_file}")
        sys.exit(1)
    
    # コンバーター初期化
    converter = EbcdicBatchConverter()
    
    print("=" * 60)
    print("EBCDIC to ASCII/Japanese Converter")
    print("=" * 60)
    print(f"Input file: {input_file}")
    print(f"Output file: {output_file}")
    print(f"Encoding: {encoding}")
    print(f"SOSI processing: Enabled")
    print(f"Record length: 80 bytes")
    
    if encoding == 'JP':
        print(f"Output encoding: Shift-JIS (Japanese)")
    else:
        print(f"Output encoding: UTF-8")
    
    print("-" * 60)
    
    try:
        # ファイル変換実行
        result = converter.convert_ebcdic_file(
            input_path=input_file,
            output_path=output_file,
            encoding=encoding,
            sosi_flag=True,      # SOSI処理有効化
            out_sosi_flag=False, # SOSIコード削除
            rlen=80              # ソースファイル標準長
        )
        
        print("\n" + "=" * 60)
        print("CONVERSION COMPLETED SUCCESSFULLY!")
        print("=" * 60)
        print(f"[OK] 出力保存先: {output_file}")
        
        # ファイルサイズ確認
        file_size = os.path.getsize(output_file)
        print(f"[INFO] ファイルサイズ: {file_size:,} bytes")
        
        # 行数確認
        with open(output_file, 'r', encoding='shift_jis' if encoding == 'JP' else 'utf-8') as f:
            line_count = sum(1 for _ in f)
        print(f"[INFO] 総行数: {line_count}")
        
        # サンプル出力 (最初の5行)
        print(f"\n[SAMPLE] 最初の5行:")
        print("-" * 40)
        with open(output_file, 'r', encoding='shift_jis' if encoding == 'JP' else 'utf-8') as f:
            for i, line in enumerate(f):
                if i >= 5:
                    break
                print(f"{i+1:2d}: {line.rstrip()}")
        
        if encoding == 'JP':
            print(f"\n[JP] 日本語文字用Shift-JISエンコーディングでファイル保存")
            print(f"     正しく表示するには、Shift-JIS対応テキストエディタを使用してください")
        
    except Exception as e:
        print(f"\n[エラー] 変換中にエラーが発生しました: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()