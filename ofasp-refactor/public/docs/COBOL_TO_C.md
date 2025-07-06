# COBOL TO C 変換ツール

COBOL プログラムを C 言語に変換するツールです。

## 概要

COBOLプログラムをCプログラムに変換することで、システムレベルのプログラミングやパフォーマンス重視のアプリケーション開発が可能になります。

## 特徴

- 高速実行: Cの高いパフォーマンス
- システムプログラミング: OS直接アクセス
- メモリ管理: 詳細なメモリ制御
- ポータビリティ: 多くのプラットフォームで実行可能

## 使用方法

```bash
node /data/cobol-converter.js input.cob output.c --target=c
```

## 変換例

**COBOL:**
```cobol
DISPLAY "Hello, World!"
```

**C:**
```c
#include <stdio.h>

int main() {
    printf("Hello, World!\n");
    return 0;
}
```

## 対応機能

- 基本入出力
- 変数定義
- 算術演算
- 条件分岐
- ループ処理

*詳細なドキュメントは準備中です。*

---
**OpenASP Project Team - 2025**