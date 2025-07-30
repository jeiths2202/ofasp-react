# COBOL TO SHELL 変換ツール

COBOL プログラムを Shell Script に変換するツールです。

## 概要

COBOLプログラムをShellスクリプトに変換することで、システム管理タスクや自動化処理として活用できます。

## 特徴

- システム統合: Unix/Linuxシステムとの親和性
- 自動化: バッチ処理・タスクスケジューリング
- 軽量実行: 追加ランタイム不要
- パイプライン: コマンド連携処理

## 使用方法

```bash
node /data/cobol-converter.js input.cob output.sh --target=shell
```

## 変換例

**COBOL:**
```cobol
DISPLAY "Processing started"
ACCEPT WS-INPUT
DISPLAY "Input received: " WS-INPUT
```

**Shell:**
```bash
#!/bin/bash
echo "Processing started"
read WS_INPUT
echo "Input received: $WS_INPUT"
```

## 対応機能

- 画面入出力
- 変数操作
- 条件分岐
- ファイル処理
- システムコマンド実行

*詳細なドキュメントは準備中です。*

---
**OpenASP Project Team - 2025**