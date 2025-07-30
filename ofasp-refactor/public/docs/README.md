# OpenASP プロジェクト

## 概要

OpenASP (Open Advanced System Products) は、レガシーシステムのモダナイゼーションを支援するオープンソースプロジェクトです。

### 主な機能

- **COBOLリファクタリング**: レガシーCOBOLコードを現代的なJavaに変換
- **CLリファクタリング**: CLスクリプトをシェルスクリプトやPythonに変換
- **Webベース管理**: 直感的なWeb UIで変換プロセスを管理

### システム要件

- Node.js 14.0以上
- Java Runtime Environment 8以上
- 最新のWebブラウザ（Chrome, Firefox, Safari, Edge）

## アーキテクチャ

### フロントエンド
- **React**: TypeScript + Hooks
- **Tailwind CSS**: レスポンシブUI
- **React Router**: ページルーティング
- **Hero Icons**: アイコン

### バックエンド
- **Node.js**: サーバーサイドロジック
- **Express**: RESTful API
- **Multer**: ファイルアップロード
- **Child Process**: Java実行

### 開発環境
- **Docker**: コンテナ化
- **VS Code**: 統合開発環境
- **Git**: バージョン管理

## プロジェクト構成

```
ofasp-refactor/
├── public/                 # 静的ファイル
│   ├── docs/              # ドキュメント
│   └── SMED_FILES/        # SMEDファイル
├── src/                   # ソースコード
│   ├── components/        # Reactコンポーネント
│   ├── pages/             # ページコンポーネント
│   ├── hooks/             # カスタムフック
│   ├── i18n/              # 国際化
│   └── utils/             # ユーティリティ
├── server/                # サーバーサイド
├── docs/                  # プロジェクトドキュメント
└── data/                  # データファイル
```

## 主要機能

### 1. COBOLリファクタリング
- COBOL-to-Java変換
- 構文解析とエラー検出
- インタラクティブな実行環境
- ファイル入出力サポート

### 2. CLリファクタリング
- CLスクリプトの分析
- シェルスクリプトへの変換
- コマンド互換性チェック

### 3. Web管理インターフェース
- ドラッグ&ドロップファイル操作
- リアルタイムプレビュー
- 変換結果のダウンロード
- 実行ログの表示

## 対応言語

### 国際化対応
- 日本語 (ja) - メイン
- 韓国語 (ko) - サポート

### 技術サポート
- COBOL (IBMスタイル)
- Java 8+
- CL (AS/400)
- Shell Script

## インストール

### 1. 依存関係のインストール
```bash
npm install
```

### 2. 開発サーバーの起動
```bash
npm start
```

### 3. プロダクションビルド
```bash
npm run build
```

## 使用方法

### COBOLリファクタリング
1. 左メニューから「COBOLリファクタリング」を選択
2. COBOLファイルをアップロード
3. 「変換」ボタンでJavaコードを生成
4. 「実行」ボタンでコードをテスト

### CLリファクタリング
1. 左メニューから「CLリファクタリング」を選択
2. CLファイルをアップロード
3. 変換オプションを選択
4. 変換結果をダウンロード

## 設定

### 環境変数
```bash
REACT_APP_DEFAULT_LANG=ja
REACT_APP_API_URL=http://localhost:3001
```

### 設定ファイル
- `src/asp.conf`: ASP設定
- `src/i18n/`: 翻訳ファイル
- `docs/`: ドキュメント

## 開発

### 開発環境セットアップ
```bash
# リポジトリのクローン
git clone https://github.com/openaspproject/ofasp-refactor.git

# 依存関係のインストール
cd ofasp-refactor
npm install

# 開発サーバーの起動
npm start
```

### コーディング規約
- TypeScript strict mode
- ESLint + Prettier
- Tailwind CSS
- React Hooks パターン

## トラブルシューティング

### よくある問題

#### 1. Javaコンパイルエラー
```bash
# Java環境の確認
java -version
javac -version
```

#### 2. ファイルアップロードエラー
- ファイルサイズ制限: 10MB
- 対応拡張子: .cob, .cobol, .cl, .txt

#### 3. 変換エラー
- 未対応COBOL構文の確認
- エラーログの詳細確認

## ライセンス

このプロジェクトはMITライセンスの下で公開されています。

## 貢献

プロジェクトへの貢献を歓迎します。

### 貢献方法
1. Issue の作成
2. Fork してブランチを作成
3. 変更を実装
4. Pull Request を送信

## サポート

- GitHub Issues: バグ報告・機能要求
- Documentation: 詳細なドキュメント
- Examples: サンプルコード

---

**OpenASP Project Team - 2025**