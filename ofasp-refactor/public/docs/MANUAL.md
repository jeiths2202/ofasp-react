# OpenASP ユーザーマニュアル

OpenASP (Open Advanced System Products) の包括的なユーザーマニュアルです。

## 目次

1. [はじめに](#はじめに)
2. [システム要件](#システム要件)
3. [インストール](#インストール)
4. [基本操作](#基本操作)
5. [機能詳細](#機能詳細)
6. [トラブルシューティング](#トラブルシューティング)
7. [FAQ](#faq)

## はじめに

OpenASPは、レガシーシステムのモダナイゼーションを支援するオープンソースプロジェクトです。主にCOBOLやCLなどの古い言語で書かれたプログラムを、現代的なJava、Shell Script、Pythonなどに変換することができます。

### 主な特徴

- **多言語対応**: COBOL、CLから複数の現代言語への変換
- **Web UI**: 直感的なブラウザベースのインターフェース
- **リアルタイム変換**: 即座に変換結果を確認
- **エラー検出**: 変換不可能な構文の事前検出
- **日本語サポート**: 完全な日本語インターフェース

## システム要件

### 最小要件
- **OS**: Windows 10、macOS 10.15、Linux Ubuntu 18.04以上
- **Node.js**: v12.0以上
- **Java**: JRE 8以上（Java変換時）
- **メモリ**: 4GB RAM以上
- **ストレージ**: 1GB以上の空き容量

### 推奨要件
- **OS**: Windows 11、macOS 12.0、Linux Ubuntu 20.04以上
- **Node.js**: v16.0以上
- **Java**: JDK 11以上
- **メモリ**: 8GB RAM以上
- **ストレージ**: 5GB以上の空き容量

### 対応ブラウザ
- Chrome 90以上
- Firefox 88以上
- Safari 14以上
- Edge 90以上

## インストール

### 1. Node.js のインストール

公式サイトから最新版をダウンロードしてインストール:
https://nodejs.org/

### 2. プロジェクトの取得

```bash
git clone https://github.com/openaspproject/ofasp-refactor.git
cd ofasp-refactor
```

### 3. 依存関係のインストール

```bash
npm install
```

### 4. 開発サーバーの起動

```bash
npm start
```

### 5. ブラウザでアクセス

http://localhost:3005 にアクセス

## 基本操作

### 1. ダッシュボード

アプリケーション起動時に表示される画面です。

- **概要**: プロジェクトの説明
- **機能紹介**: COBOLリファクタリング、CLリファクタリング
- **プロセス**: 変換の流れ

### 2. ファイルのアップロード

#### 方法1: ドラッグ&ドロップ
1. エクスプローラーからファイルを選択
2. アップロード領域にドラッグ
3. ファイルをドロップ

#### 方法2: ファイル選択
1. 「ファイルを選択」ボタンをクリック
2. ファイルブラウザーでファイルを選択
3. 「開く」をクリック

### 3. 変換の実行

1. ファイルアップロード後、「変換」ボタンをクリック
2. 変換処理の進行状況を確認
3. 変換結果をプレビューで確認

### 4. 結果のダウンロード

1. 変換完了後、「ダウンロード」ボタンをクリック
2. 変換されたファイルを保存

### 5. 実行とテスト

1. 「実行」ボタンでコードをテスト
2. ターミナル画面で実行結果を確認
3. 必要に応じて入力操作

## 機能詳細

### COBOLリファクタリング

#### 対応する変換先言語
- **Java**: エンタープライズアプリケーション向け
- **C**: システムプログラミング向け
- **Shell Script**: システム管理スクリプト向け
- **Python**: データ処理・AI向け

#### サポートされるCOBOL機能
- プログラム構造 (PROGRAM-ID, DIVISION)
- データ定義 (WORKING-STORAGE SECTION)
- 手続き処理 (PROCEDURE DIVISION)
- 基本文 (DISPLAY, ACCEPT, MOVE, COMPUTE)
- 制御構造 (IF, EVALUATE, PERFORM)
- ファイル操作 (OPEN, READ, WRITE, CLOSE)

#### 変換例

**COBOL (入力):**
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO.

PROCEDURE DIVISION.
    DISPLAY "Hello, World!"
    STOP RUN.
```

**Java (出力):**
```java
public class Hello {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
    }
}
```

### CLリファクタリング

#### 対応する変換先言語
- **Shell Script**: Unix/Linux環境向け
- **JavaScript**: Node.js環境向け
- **Python**: 汎用スクリプト向け

#### サポートされるCL機能
- プログラム制御 (PGM, ENDPGM)
- 変数定義 (DCL VAR)
- コマンド実行 (CALL PGM, SBMJOB)
- 条件分岐 (IF, ELSE, ENDIF)
- ループ処理 (DOWHILE, ENDDO)

### ターミナル機能

#### 基本操作
- **ドラッグ移動**: タイトルバーをドラッグしてウィンドウ移動
- **サイズ変更**: 端をドラッグしてウィンドウサイズ調整
- **スクロール**: マウスホイールまたはスクロールバー

#### コマンド実行
- Java: `javac ファイル名.java` → `java クラス名`
- Python: `python スクリプト名.py`
- Shell: `bash スクリプト名.sh`

#### 入力処理
- プログラムがユーザー入力を要求する場合、入力欄が表示
- 値を入力してEnterキーで送信

### 設定とカスタマイズ

#### 言語設定
1. 右上の設定アイコンをクリック
2. 言語を選択（日本語/韓国語）
3. 設定を保存

#### テーマ設定
1. 右上のテーマ切り替えボタンをクリック
2. ライト/ダークモードを選択

#### 詳細設定
- エンコーディング設定
- 出力フォーマット設定
- エラー表示レベル設定

## トラブルシューティング

### よくある問題

#### 1. アップロードできない

**症状**: ファイルをドロップしても反応しない

**原因と解決方法**:
- ファイルサイズが大きすぎる → 10MB以下のファイルを使用
- 対応していない拡張子 → .cob, .cobol, .cl, .txt ファイルを使用
- ブラウザの問題 → ページを再読み込み

#### 2. 変換に失敗する

**症状**: 変換ボタンを押してもエラーが表示される

**原因と解決方法**:
- 未対応の構文が含まれている → エラーメッセージを確認し、対応構文に修正
- ファイルが破損している → 元のファイルを確認
- メモリ不足 → ブラウザを再起動

#### 3. 実行時エラー

**症状**: 変換されたコードの実行時にエラーが発生

**原因と解決方法**:
- Javaがインストールされていない → JDKをインストール
- 環境変数が設定されていない → PATH環境変数を確認
- ファイルアクセス権限の問題 → 実行権限を付与

#### 4. 日本語が文字化けする

**症状**: 日本語テキストが正しく表示されない

**原因と解決方法**:
- エンコーディングの問題 → UTF-8またはShift_JISを確認
- フォント設定の問題 → ブラウザのフォント設定を確認

### エラーコード一覧

| コード | 意味 | 対処法 |
|--------|------|--------|
| E001 | ファイル読み込みエラー | ファイルパスとアクセス権限を確認 |
| E002 | 構文解析エラー | COBOL/CLの構文を確認 |
| E003 | 変換エラー | 未対応機能の使用を確認 |
| E004 | 出力エラー | 出力先の権限とディスク容量を確認 |
| E005 | 実行エラー | 実行環境の設定を確認 |

### ログファイルの確認

#### ブラウザの開発者ツール
1. F12キーを押して開発者ツールを開く
2. Consoleタブでエラーメッセージを確認
3. Networkタブで通信エラーを確認

#### サーバーログ
```bash
# 開発サーバーのログを確認
npm start 2>&1 | tee server.log
```

## FAQ

### Q1. どのような種類のCOBOLファイルに対応していますか？

A1. 以下の形式に対応しています：
- IBM COBOL (Enterprise COBOL)
- Micro Focus COBOL
- Open COBOL/GnuCOBOL
- ACUCOBOL
- 拡張子: .cob, .cobol, .txt

### Q2. 変換したJavaコードはそのまま本番環境で使用できますか？

A2. 基本的な機能については本番使用可能ですが、以下の点にご注意ください：
- パフォーマンステストを実施
- セキュリティレビューを実施
- 例外処理の追加検討
- ログ機能の追加検討

### Q3. 大きなCOBOLファイルの処理時間はどのくらいかかりますか？

A3. ファイルサイズと複雑さによって異なります：
- 小さなファイル (1KB未満): 数秒
- 中程度のファイル (10KB-100KB): 数十秒
- 大きなファイル (1MB以上): 数分

### Q4. 変換に失敗した場合、どこを修正すればよいですか？

A4. エラーメッセージに表示される情報を確認してください：
- 未対応機能のリスト
- 問題のある行番号
- 推奨される代替手法

### Q5. コマンドラインツールはありますか？

A5. はい、以下のコマンドラインツールが利用可能です：
```bash
node /data/cobol-converter.js input.cob output.java
```
詳細は[COBOL TO JAVAリファレンス](COBOL_TO_JAVA.md)をご覧ください。

### Q6. 複数ファイルの一括変換は可能ですか？

A6. 現在のバージョンでは単一ファイルの変換のみサポートしています。一括変換機能は次期バージョンで対応予定です。

### Q7. 変換結果をカスタマイズできますか？

A7. 以下の方法でカスタマイズ可能です：
- 設定ファイルでテンプレートを変更
- カスタム変換ルールの追加
- 後処理スクリプトの実行

### Q8. セキュリティ面での考慮事項はありますか？

A8. 以下の点にご注意ください：
- 機密データを含むファイルのアップロード時の注意
- 生成されたコードのセキュリティレビュー
- 本番環境での適切なアクセス制御

### Q9. サポートとコミュニティはありますか？

A9. 以下のサポートチャネルが利用可能です：
- GitHub Issues: バグ報告・機能要求
- ドキュメント: 包括的なガイド
- コミュニティフォーラム: ユーザー同士の情報交換

### Q10. ライセンスと商用利用について教えてください。

A10. OpenASPはMITライセンスで公開されており、以下が可能です：
- 商用利用
- 修正・改変
- 再配布
- プライベート利用

## 付録

### A. キーボードショートカット

| ショートカット | 機能 |
|-------------|------|
| Ctrl + O | ファイルを開く |
| Ctrl + S | ファイルを保存 |
| Ctrl + Enter | 変換実行 |
| F5 | ページリロード |
| F11 | フルスクリーン切り替え |
| Ctrl + + | フォントサイズ拡大 |
| Ctrl + - | フォントサイズ縮小 |

### B. 設定ファイル

#### `.aspconfig`
```json
{
  "version": "1.0.0",
  "defaultLanguage": "ja",
  "defaultTheme": "dark",
  "encoding": "utf-8",
  "outputFormat": "java",
  "enableVerboseLogging": false
}
```

#### `package.json`
```json
{
  "name": "ofasp-refactor",
  "version": "1.0.0",
  "description": "OpenASP Legacy System Refactoring Tool",
  "main": "src/index.js",
  "scripts": {
    "start": "react-scripts start",
    "build": "react-scripts build",
    "test": "react-scripts test"
  }
}
```

### C. APIリファレンス

#### REST API エンドポイント

**ファイルアップロード**
```
POST /api/upload
Content-Type: multipart/form-data
```

**変換実行**
```
POST /api/convert
Content-Type: application/json
{
  "fileId": "string",
  "targetLanguage": "java|python|shell|c",
  "options": {}
}
```

**変換結果取得**
```
GET /api/convert/{conversionId}
Response: {
  "status": "success|error|processing",
  "result": "string",
  "errors": []
}
```

### D. サンプルコード集

#### COBOL サンプル
- [税計算プログラム](../samples/tax-calculation.cob)
- [給与計算プログラム](../samples/payroll.cob)
- [在庫管理プログラム](../samples/inventory.cob)

#### CL サンプル
- [バックアップスクリプト](../samples/backup.cl)
- [ユーザー管理スクリプト](../samples/user-mgmt.cl)
- [システム監視スクリプト](../samples/monitor.cl)

---

**OpenASP Project Team - 2025**
*最終更新: 2025年7月6日*

このマニュアルについてご質問やフィードバックがございましたら、GitHub Issuesまでお知らせください。