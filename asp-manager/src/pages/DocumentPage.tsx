import React from 'react';
import MarkdownRenderer from '../components/MarkdownRenderer';

interface DocumentPageProps {
  isDarkMode: boolean;
}

const DocumentPage: React.FC<DocumentPageProps> = ({ isDarkMode }) => {
  const sampleMarkdown = `# ASP Manager ドキュメント

## 概要
ASP Managerは、OpenASPシステムの管理インターフェースです。

### 主な機能
- **ダッシュボード**: システムの概要を一目で確認
- **ユーザー管理**: アカウントの作成・編集・削除
- **SMEDマップ管理**: 画面定義の管理
- **プログラム管理**: 各種プログラムの設定

## コード例

\`\`\`javascript
// APIへの接続例
const connectToAPI = async () => {
  const response = await fetch('http://localhost:8000/api/health');
  const data = await response.json();
  return data;
};
\`\`\`

## テーブル例

| 機能 | 説明 | ステータス |
|------|------|------------|
| ダッシュボード | システム概要表示 | ✅ 完了 |
| ユーザー管理 | アカウント管理 | 🚧 開発中 |
| SMED管理 | 画面定義管理 | 📋 計画中 |

## 画像の表示
![サンプル画像](https://via.placeholder.com/600x300)

> **注意**: このドキュメントはサンプルです。実際の内容は後で更新されます。
`;

  return (
    <div className="max-w-4xl mx-auto p-8">
      <MarkdownRenderer content={sampleMarkdown} isDarkMode={isDarkMode} />
    </div>
  );
};

export default DocumentPage;