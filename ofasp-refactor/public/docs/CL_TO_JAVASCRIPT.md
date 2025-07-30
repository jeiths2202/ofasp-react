# CL TO JavaScript 変換ツール

CL (Control Language) プログラムを JavaScript に変換するツールです。

## 概要

CLプログラムをNode.js JavaScriptに変換し、WebアプリケーションやAPIサーバーとして活用できます。

## 特徴

- Web統合: Express.js、React、Vue.js等との連携
- API開発: RESTful API、GraphQL API
- 非同期処理: Promise、async/await
- NPMエコシステム: 豊富なライブラリ活用

## 使用方法

```bash
node /data/cl-converter.js input.cl output.js --target=javascript
```

## 変換例

**CL:**
```cl
PGM
  DCL VAR(&MSG) TYPE(*CHAR) LEN(50)
  CHGVAR VAR(&MSG) VALUE('Hello from CL')
  SNDPGMMSG MSG(&MSG)
ENDPGM
```

**JavaScript:**
```javascript
async function main() {
    let msg = 'Hello from CL';
    console.log(msg);
}

main().catch(console.error);
```

## 対応機能

### 基本制御
- 変数定義・操作
- 条件分岐 (IF/ELSE)
- ループ処理
- 関数呼び出し

### ファイル操作
- ファイル読み書き
- ディレクトリ操作
- ファイル情報取得

### Web機能
- HTTP API呼び出し
- JSON処理
- 非同期処理
- エラーハンドリング

## Express.js統合例

```javascript
const express = require('express');
const app = express();

// CL変換された関数
async function processData(input) {
    // 元のCLロジック
    return result;
}

app.post('/api/process', async (req, res) => {
    try {
        const result = await processData(req.body);
        res.json({ success: true, data: result });
    } catch (error) {
        res.status(500).json({ error: error.message });
    }
});
```

*詳細なドキュメントは準備中です。*

---
**OpenASP Project Team - 2025**