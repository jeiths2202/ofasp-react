# CL TO Python 変換ツール

CL (Control Language) プログラムを Python に変換するツールです。

## 概要

CLプログラムをPythonに変換し、データ分析、自動化、Web開発等に活用できます。

## 特徴

- データ分析: Pandas、NumPy、matplotlib
- 自動化: スクリプト自動実行、タスクスケジューリング
- Web開発: Django、Flask
- AI/ML: scikit-learn、TensorFlow、PyTorch

## 使用方法

```bash
node /data/cl-converter.js input.cl output.py --target=python
```

## 変換例

**CL:**
```cl
PGM
  DCL VAR(&COUNT) TYPE(*DEC) LEN(5 0)
  DCL VAR(&INDEX) TYPE(*DEC) LEN(3 0)
  CHGVAR VAR(&COUNT) VALUE(0)
  DOWHILE COND(&INDEX *LE 10)
    CHGVAR VAR(&COUNT) VALUE(&COUNT + &INDEX)
    CHGVAR VAR(&INDEX) VALUE(&INDEX + 1)
  ENDDO
  SNDPGMMSG MSG('Total:' *CAT %CHAR(&COUNT))
ENDPGM
```

**Python:**
```python
def main():
    count = 0
    index = 1
    
    while index <= 10:
        count = count + index
        index = index + 1
    
    print(f"Total: {count}")

if __name__ == "__main__":
    main()
```

## 対応機能

### 基本制御
- 変数操作
- 条件分岐
- ループ処理
- 関数定義

### ファイル処理
- テキストファイル読み書き
- CSVファイル処理
- JSONファイル処理
- ディレクトリ操作

### システム統合
- OS コマンド実行
- 環境変数操作
- プロセス制御
- ログ出力

## データ分析統合例

```python
import pandas as pd
import numpy as np

def process_data_file(filename):
    """CL変換されたデータ処理関数"""
    # データ読み込み
    df = pd.read_csv(filename)
    
    # 元のCLロジックをPandasで実装
    result = df.groupby('category').sum()
    
    return result

def generate_report():
    """レポート生成（元CLのレポート機能）"""
    data = process_data_file('input.csv')
    data.to_excel('report.xlsx')
    print("Report generated successfully")
```

## Web API統合例

```python
from flask import Flask, request, jsonify

app = Flask(__name__)

@app.route('/api/process', methods=['POST'])
def process_cl_logic():
    """CL変換されたビジネスロジック"""
    data = request.json
    
    # 元のCLプログラムロジック
    result = perform_calculation(data)
    
    return jsonify({
        'success': True,
        'result': result
    })
```

*詳細なドキュメントは準備中です。*

---
**OpenASP Project Team - 2025**