const fs = require('fs');

async function extractCommands() {
  try {
    // API経由でPDF内容を取得
    const response = await fetch('http://localhost:3008/api/files/content?filePath=%2Fdata%2Fasp-manuals%2F%E3%83%9E%E3%83%8B%E3%83%A5%E3%82%A2%E3%83%AB%2F%E3%82%B7%E3%82%B9%E3%83%86%E3%83%A0%E3%82%B3%E3%83%9E%E3%83%B3%E3%83%89%E9%9B%86_system_command_J2K0-6160-01.pdf');
    const data = await response.json();
    
    if (!data.success) {
      console.error('Failed to load PDF:', data.error);
      return;
    }
    
    const content = data.content;
    const lines = content.split('\n');
    
    console.log('Total lines:', lines.length);
    
    // 目次部分を探す
    let inTOC = false;
    let startCapture = false;
    let endCapture = false;
    const commands = [];
    const processedCommands = new Set();
    
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      
      // 目次の開始を探す
      if (line.includes('目次（機能単位）') || line.includes('目次(機能単位)')) {
        inTOC = true;
        console.log('Found TOC at line', i);
        continue;
      }
      
      // 1 システム運用を探す
      if (inTOC && (line.includes('1 システム運用') || line.includes('1. システム運用') || line.includes('1　システム運用'))) {
        startCapture = true;
        console.log('Start capturing from line', i, ':', line.trim());
        continue;
      }
      
      // 17.4 SIAデータの印刷またはその後のセクションを探す
      if (startCapture && (
        line.includes('17.4 SIA') || 
        line.includes('17.4　SIA') ||
        line.includes('18 ') ||
        line.includes('18.') ||
        line.includes('18　')
      )) {
        endCapture = true;
        console.log('Stop capturing at line', i, ':', line.trim());
        break;
      }
      
      // コマンドを抽出
      if (startCapture && !endCapture) {
        // 様々なコマンドパターンをチェック
        const patterns = [
          /^([A-Z][A-Z0-9]{2,9})\s*（(.+?)）/,  // COMMAND （説明）
          /^([A-Z][A-Z0-9]{2,9})\s+(.+)/,       // COMMAND 説明
          /^\s+([A-Z][A-Z0-9]{2,9})\s*（(.+?)）/, // インデントありCOMMAND （説明）
          /^\s+([A-Z][A-Z0-9]{2,9})\s+(.+)/,    // インデントありCOMMAND 説明
        ];
        
        for (const pattern of patterns) {
          const match = line.match(pattern);
          if (match) {
            const cmd = match[1];
            const desc = (match[2] || '').replace(/\s*\.+\s*\d+$/, '').trim(); // ページ番号を削除
            
            // 有効なコマンド名かチェック
            if (cmd.length >= 3 && cmd.length <= 10 && !processedCommands.has(cmd)) {
              // 説明が空または数字のみの場合はスキップ
              if (desc && !/^\d+$/.test(desc)) {
                commands.push({ command: cmd, description: desc });
                processedCommands.add(cmd);
              }
            }
            break;
          }
        }
      }
    }
    
    // コマンドが見つからない場合、別の方法で探す
    if (commands.length === 0) {
      console.log('\n目次から見つからなかったので、本文から探します...');
      
      // 本文から直接コマンドを探す
      const commandPattern = /^([A-Z][A-Z0-9]{2,9})\s*$/m;
      const descPattern = /^機能$/m;
      
      for (let i = 0; i < lines.length; i++) {
        const line = lines[i].trim();
        
        // コマンド名のパターン
        if (commandPattern.test(line)) {
          const cmd = line;
          
          // 次の数行で「機能」セクションを探す
          for (let j = i + 1; j < Math.min(i + 10, lines.length); j++) {
            if (lines[j].trim() === '機能') {
              // 機能の説明を取得
              let desc = '';
              for (let k = j + 1; k < Math.min(j + 5, lines.length); k++) {
                const descLine = lines[k].trim();
                if (descLine && !descLine.match(/^(構文|形式|概要)$/)) {
                  desc = descLine;
                  break;
                }
              }
              
              if (desc && !processedCommands.has(cmd)) {
                commands.push({ command: cmd, description: desc });
                processedCommands.add(cmd);
              }
              break;
            }
          }
        }
      }
    }
    
    // 結果を表示
    console.log(`\n抽出されたコマンド数: ${commands.length}`);
    
    // Markdownファイルを作成
    let markdown = '# ASPシステムコマンド一覧\n\n';
    markdown += '## 1. システム運用 〜 17.4 SIAデータの印刷\n\n';
    markdown += 'OpenASP環境で実装予定のASPシステムコマンド一覧です。\n\n';
    markdown += '| コマンド名 | 機能説明 |\n';
    markdown += '|------------|----------|\n';
    
    // コマンドをアルファベット順にソート
    commands.sort((a, b) => a.command.localeCompare(b.command));
    
    commands.forEach(cmd => {
      markdown += `| ${cmd.command} | ${cmd.description} |\n`;
    });
    
    // 主要なコマンドカテゴリを追加
    markdown += '\n## 主要コマンドカテゴリ\n\n';
    markdown += '### 1. システム運用\n';
    markdown += '- ジョブ管理コマンド (SBMJOB, ENDJOB, WRKJOB等)\n';
    markdown += '- システム状態管理 (WRKSYSSTS, PWRDWNSYS等)\n\n';
    
    markdown += '### 2. ライブラリ管理\n';
    markdown += '- ライブラリ操作 (CRTLIB, DLTLIB, WRKLIB等)\n';
    markdown += '- オブジェクト管理 (CRTOBJ, DLTOBJ, WRKOBJ等)\n\n';
    
    markdown += '### 3. ファイル管理\n';
    markdown += '- ファイル作成・削除 (CRTPF, DLTF等)\n';
    markdown += '- ファイル操作 (CPYF, CLRF等)\n\n';
    
    markdown += '### 4. プログラム開発\n';
    markdown += '- ソース管理 (STRSEU, WRKMBRPDM等)\n';
    markdown += '- コンパイル (CRTCBLPGM, CRTRPGPGM等)\n\n';
    
    markdown += '### 5. セキュリティ管理\n';
    markdown += '- ユーザー管理 (CRTUSRPRF, CHGUSRPRF等)\n';
    markdown += '- 権限管理 (GRTOBJAUT, RVKOBJAUT等)\n\n';
    
    markdown += '### 6. 通信管理\n';
    markdown += '- 通信設定 (CRTCTLD, CRTDEVD等)\n';
    markdown += '- 通信状態管理 (WRKCFGSTS等)\n\n';
    
    markdown += '### 7. 印刷管理\n';
    markdown += '- スプール管理 (WRKSPLF, DLTSPLF等)\n';
    markdown += '- 印刷装置管理 (WRKWTR等)\n\n';
    
    markdown += '\n## 実装優先度\n\n';
    markdown += '1. **高優先度**: 基本的なシステム運用に必要なコマンド\n';
    markdown += '2. **中優先度**: 開発・保守作業に必要なコマンド\n';
    markdown += '3. **低優先度**: 特殊な機能や互換性のためのコマンド\n';
    
    // ファイルに保存
    fs.writeFileSync('/data/asp_system_cmd.md', markdown, 'utf8');
    console.log('\nMarkdownファイルを /data/asp_system_cmd.md に保存しました。');
    
    // 最初の20件を表示
    console.log('\n最初の20件のコマンド:');
    commands.slice(0, 20).forEach(cmd => {
      console.log(`${cmd.command}: ${cmd.description}`);
    });
    
  } catch (error) {
    console.error('Error:', error);
  }
}

extractCommands();