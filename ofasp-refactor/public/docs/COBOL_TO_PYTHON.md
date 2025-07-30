# COBOL TO Python 変換ツール

COBOL プログラムを Python に変換するツールです。

## 概要

COBOLプログラムをPythonに変換することで、データ分析、AI/ML、Web開発など現代的な用途に活用できます。

## 特徴

- 豊富なライブラリ: NumPy、Pandas、scikit-learn等
- データ処理: CSV、JSON、データベース連携
- AI/ML統合: 機械学習モデルとの連携
- Web開発: Flask、Django等のフレームワーク

## 使用方法

```bash
node /data/cobol-converter.js input.cob output.py --target=python
```

## 変換例

**COBOL:**
```cobol
COMPUTE WS-RESULT = WS-A + WS-B * WS-C
DISPLAY "Result: " WS-RESULT
```

**Python:**
```python
ws_result = ws_a + ws_b * ws_c
print(f"Result: {ws_result}")
```

## 対応機能

- 数値計算
- 文字列処理
- ファイル入出力
- データ構造
- 制御フロー

## 拡張機能

- Pandas連携: DataFrameでの表形式データ処理
- NumPy連携: 高速数値計算
- matplotlib連携: グラフ・可視化

*詳細なドキュメントは準備中です。*

---
**OpenASP Project Team - 2025**