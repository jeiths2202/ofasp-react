const fs = require('fs');

// ASPシステムコマンドの一覧（マニュアルの目次に基づく）
const commands = [
  // 1. システム運用
  { command: 'SBMJOB', description: 'ジョブの投入' },
  { command: 'ENDJOB', description: 'ジョブの終了' },
  { command: 'WRKJOB', description: 'ジョブの処理' },
  { command: 'WRKACTJOB', description: '活動ジョブの処理' },
  { command: 'WRKSBMJOB', description: '投入済みジョブの処理' },
  { command: 'WRKUSRJOB', description: 'ユーザージョブの処理' },
  { command: 'HLDJOB', description: 'ジョブの保留' },
  { command: 'RLSJOB', description: 'ジョブの解放' },
  { command: 'CHGJOB', description: 'ジョブの変更' },
  { command: 'DSPJOB', description: 'ジョブの表示' },
  { command: 'PWRDWNSYS', description: 'システムの電源切断' },
  { command: 'WRKSYSSTS', description: 'システム状況の処理' },
  { command: 'DSPSYSSTS', description: 'システム状況の表示' },
  
  // 2. ジョブ記述
  { command: 'CRTJOBD', description: 'ジョブ記述の作成' },
  { command: 'CHGJOBD', description: 'ジョブ記述の変更' },
  { command: 'DSPJOBD', description: 'ジョブ記述の表示' },
  { command: 'DLTJOBD', description: 'ジョブ記述の削除' },
  { command: 'WRKJOBD', description: 'ジョブ記述の処理' },
  
  // 3. ライブラリ管理
  { command: 'CRTLIB', description: 'ライブラリの作成' },
  { command: 'DLTLIB', description: 'ライブラリの削除' },
  { command: 'CHGLIB', description: 'ライブラリの変更' },
  { command: 'DSPLIB', description: 'ライブラリの表示' },
  { command: 'WRKLIB', description: 'ライブラリの処理' },
  { command: 'ADDLIBLE', description: 'ライブラリリストへの追加' },
  { command: 'RMVLIBLE', description: 'ライブラリリストからの除去' },
  { command: 'EDTLIBL', description: 'ライブラリリストの編集' },
  { command: 'DSPLIBL', description: 'ライブラリリストの表示' },
  
  // 4. オブジェクト管理
  { command: 'CRTOBJ', description: 'オブジェクトの作成' },
  { command: 'DLTOBJ', description: 'オブジェクトの削除' },
  { command: 'CHGOBJ', description: 'オブジェクトの変更' },
  { command: 'DSPOBJ', description: 'オブジェクトの表示' },
  { command: 'WRKOBJ', description: 'オブジェクトの処理' },
  { command: 'MOVOBJ', description: 'オブジェクトの移動' },
  { command: 'CPYOBJ', description: 'オブジェクトの複写' },
  { command: 'RNMOBJ', description: 'オブジェクトの名前変更' },
  { command: 'ALCOBJ', description: 'オブジェクトの割り振り' },
  { command: 'DLCOBJ', description: 'オブジェクトの割り振り解除' },
  
  // 5. ファイル管理
  { command: 'CRTPF', description: '物理ファイルの作成' },
  { command: 'CRTLF', description: '論理ファイルの作成' },
  { command: 'CRTDSPF', description: '表示装置ファイルの作成' },
  { command: 'CRTPRTF', description: '印刷装置ファイルの作成' },
  { command: 'DLTF', description: 'ファイルの削除' },
  { command: 'CHGPF', description: '物理ファイルの変更' },
  { command: 'CHGLF', description: '論理ファイルの変更' },
  { command: 'DSPFD', description: 'ファイル記述の表示' },
  { command: 'DSPFFD', description: 'ファイルフィールド記述の表示' },
  { command: 'CPYF', description: 'ファイルの複写' },
  { command: 'CLRF', description: 'ファイルのクリア' },
  { command: 'RGZPFM', description: '物理ファイルメンバーの再編成' },
  
  // 6. メンバー管理
  { command: 'ADDPFM', description: '物理ファイルメンバーの追加' },
  { command: 'ADDLFM', description: '論理ファイルメンバーの追加' },
  { command: 'CHGPFM', description: '物理ファイルメンバーの変更' },
  { command: 'CHGLFM', description: '論理ファイルメンバーの変更' },
  { command: 'RMVM', description: 'メンバーの除去' },
  { command: 'RNMM', description: 'メンバーの名前変更' },
  { command: 'CPYSRCF', description: 'ソースファイルの複写' },
  
  // 7. プログラム開発
  { command: 'STRSEU', description: 'ソース入力ユーティリティの開始' },
  { command: 'WRKMBRPDM', description: 'PDMによるメンバーの処理' },
  { command: 'CRTCBLPGM', description: 'COBOLプログラムの作成' },
  { command: 'CRTCBLMOD', description: 'COBOLモジュールの作成' },
  { command: 'CRTRPGPGM', description: 'RPGプログラムの作成' },
  { command: 'CRTRPGMOD', description: 'RPGモジュールの作成' },
  { command: 'CRTCLPGM', description: 'CLプログラムの作成' },
  { command: 'CRTPGM', description: 'プログラムの作成' },
  { command: 'CRTBNDCBL', description: 'バインドCOBOLプログラムの作成' },
  { command: 'CRTBNDRPG', description: 'バインドRPGプログラムの作成' },
  { command: 'CRTSRVPGM', description: 'サービスプログラムの作成' },
  { command: 'DLTPGM', description: 'プログラムの削除' },
  { command: 'DLTMOD', description: 'モジュールの削除' },
  { command: 'DSPPGM', description: 'プログラムの表示' },
  { command: 'WRKPGM', description: 'プログラムの処理' },
  
  // 8. コマンド管理
  { command: 'CRTCMD', description: 'コマンドの作成' },
  { command: 'CHGCMD', description: 'コマンドの変更' },
  { command: 'DLTCMD', description: 'コマンドの削除' },
  { command: 'DSPCMD', description: 'コマンドの表示' },
  { command: 'WRKCMD', description: 'コマンドの処理' },
  
  // 9. メッセージ管理
  { command: 'SNDMSG', description: 'メッセージの送信' },
  { command: 'SNDBRKMSG', description: '中断メッセージの送信' },
  { command: 'DSPMSG', description: 'メッセージの表示' },
  { command: 'WRKMSG', description: 'メッセージの処理' },
  { command: 'CRTMSGF', description: 'メッセージファイルの作成' },
  { command: 'ADDMSGD', description: 'メッセージ記述の追加' },
  { command: 'CHGMSGD', description: 'メッセージ記述の変更' },
  { command: 'RMVMSGD', description: 'メッセージ記述の除去' },
  { command: 'WRKMSGD', description: 'メッセージ記述の処理' },
  { command: 'DSPMSGD', description: 'メッセージ記述の表示' },
  
  // 10. セキュリティ管理
  { command: 'CRTUSRPRF', description: 'ユーザープロファイルの作成' },
  { command: 'CHGUSRPRF', description: 'ユーザープロファイルの変更' },
  { command: 'DLTUSRPRF', description: 'ユーザープロファイルの削除' },
  { command: 'DSPUSRPRF', description: 'ユーザープロファイルの表示' },
  { command: 'WRKUSRPRF', description: 'ユーザープロファイルの処理' },
  { command: 'GRTOBJAUT', description: 'オブジェクト権限の認可' },
  { command: 'RVKOBJAUT', description: 'オブジェクト権限の取り消し' },
  { command: 'EDTOBJAUT', description: 'オブジェクト権限の編集' },
  { command: 'DSPOBJAUT', description: 'オブジェクト権限の表示' },
  { command: 'CHGPWD', description: 'パスワードの変更' },
  
  // 11. 装置管理
  { command: 'CRTDEVDSP', description: '表示装置記述の作成' },
  { command: 'CRTDEVPRT', description: '印刷装置記述の作成' },
  { command: 'CHGDEVDSP', description: '表示装置記述の変更' },
  { command: 'CHGDEVPRT', description: '印刷装置記述の変更' },
  { command: 'DLTDEVD', description: '装置記述の削除' },
  { command: 'DSPDEVD', description: '装置記述の表示' },
  { command: 'WRKDEVD', description: '装置記述の処理' },
  { command: 'VRYCFG', description: '構成の変更' },
  
  // 12. 通信管理
  { command: 'CRTCTLD', description: '制御装置記述の作成' },
  { command: 'CRTLIND', description: '回線記述の作成' },
  { command: 'CHGCTLD', description: '制御装置記述の変更' },
  { command: 'CHGLIND', description: '回線記述の変更' },
  { command: 'DLTCTLD', description: '制御装置記述の削除' },
  { command: 'DLTLIND', description: '回線記述の削除' },
  { command: 'WRKCFGSTS', description: '構成状況の処理' },
  { command: 'STRLIND', description: '回線の開始' },
  { command: 'ENDLIND', description: '回線の終了' },
  
  // 13. スプール管理
  { command: 'WRKSPLF', description: 'スプールファイルの処理' },
  { command: 'CPYSPLF', description: 'スプールファイルの複写' },
  { command: 'DLTSPLF', description: 'スプールファイルの削除' },
  { command: 'DSPSPLF', description: 'スプールファイルの表示' },
  { command: 'CHGSPLFA', description: 'スプールファイル属性の変更' },
  { command: 'HLDSPLF', description: 'スプールファイルの保留' },
  { command: 'RLSSPLF', description: 'スプールファイルの解放' },
  
  // 14. 出力待ち行列管理
  { command: 'CRTOUTQ', description: '出力待ち行列の作成' },
  { command: 'CHGOUTQ', description: '出力待ち行列の変更' },
  { command: 'DLTOUTQ', description: '出力待ち行列の削除' },
  { command: 'WRKOUTQ', description: '出力待ち行列の処理' },
  { command: 'CLROUTQ', description: '出力待ち行列のクリア' },
  { command: 'HLDOUTQ', description: '出力待ち行列の保留' },
  { command: 'RLSOUTQ', description: '出力待ち行列の解放' },
  
  // 15. 書出しプログラム管理
  { command: 'STRPRTWTR', description: '印刷書出しプログラムの開始' },
  { command: 'ENDWTR', description: '書出しプログラムの終了' },
  { command: 'CHGWTR', description: '書出しプログラムの変更' },
  { command: 'HLDWTR', description: '書出しプログラムの保留' },
  { command: 'RLSWTR', description: '書出しプログラムの解放' },
  { command: 'WRKWTR', description: '書出しプログラムの処理' },
  
  // 16. 保管・復元
  { command: 'SAVLIB', description: 'ライブラリの保管' },
  { command: 'SAVOBJ', description: 'オブジェクトの保管' },
  { command: 'SAVCHGOBJ', description: '変更オブジェクトの保管' },
  { command: 'SAVSYS', description: 'システムの保管' },
  { command: 'RSTLIB', description: 'ライブラリの復元' },
  { command: 'RSTOBJ', description: 'オブジェクトの復元' },
  { command: 'DSPTAP', description: 'テープの表示' },
  { command: 'INZTAP', description: 'テープの初期化' },
  
  // 17. その他のユーティリティ
  { command: 'CPYTOTAP', description: 'テープへの複写' },
  { command: 'CPYFRMTAP', description: 'テープからの複写' },
  { command: 'DSPJOBLOG', description: 'ジョブログの表示' },
  { command: 'WRKJOBLOG', description: 'ジョブログの処理' },
  { command: 'DSPLOG', description: 'ログの表示' },
  { command: 'PRTSYSINF', description: 'システム情報の印刷' }
];

// Markdownファイルを作成
let markdown = '# ASPシステムコマンド一覧\n\n';
markdown += '## 1. システム運用 〜 17.4 SIAデータの印刷\n\n';
markdown += 'OpenASP環境で実装予定のASPシステムコマンド一覧です。\n\n';
markdown += '### 総コマンド数: ' + commands.length + '\n\n';

// カテゴリ別に整理
const categories = {
  'システム運用': [],
  'ジョブ記述': [],
  'ライブラリ管理': [],
  'オブジェクト管理': [],
  'ファイル管理': [],
  'メンバー管理': [],
  'プログラム開発': [],
  'コマンド管理': [],
  'メッセージ管理': [],
  'セキュリティ管理': [],
  '装置管理': [],
  '通信管理': [],
  'スプール管理': [],
  '出力待ち行列管理': [],
  '書出しプログラム管理': [],
  '保管・復元': [],
  'その他のユーティリティ': []
};

// コマンドをカテゴリ別に分類
commands.forEach(cmd => {
  // カテゴリを判定
  if (cmd.command.includes('JOB') && !cmd.command.includes('JOBD')) {
    categories['システム運用'].push(cmd);
  } else if (cmd.command.includes('JOBD')) {
    categories['ジョブ記述'].push(cmd);
  } else if (cmd.command.includes('LIB')) {
    categories['ライブラリ管理'].push(cmd);
  } else if (cmd.command.includes('OBJ') && !cmd.command.includes('CBLPGM')) {
    categories['オブジェクト管理'].push(cmd);
  } else if (cmd.command.includes('F') && (cmd.command.startsWith('CRT') || cmd.command.startsWith('CHG') || cmd.command.startsWith('DLT') || cmd.command.startsWith('DSP'))) {
    categories['ファイル管理'].push(cmd);
  } else if (cmd.command.includes('M') && (cmd.command.includes('PFM') || cmd.command.includes('LFM'))) {
    categories['メンバー管理'].push(cmd);
  } else if (cmd.command.includes('PGM') || cmd.command.includes('MOD') || cmd.command.includes('SEU') || cmd.command.includes('PDM')) {
    categories['プログラム開発'].push(cmd);
  } else if (cmd.command.includes('CMD')) {
    categories['コマンド管理'].push(cmd);
  } else if (cmd.command.includes('MSG')) {
    categories['メッセージ管理'].push(cmd);
  } else if (cmd.command.includes('USR') || cmd.command.includes('AUT') || cmd.command.includes('PWD')) {
    categories['セキュリティ管理'].push(cmd);
  } else if (cmd.command.includes('DEV')) {
    categories['装置管理'].push(cmd);
  } else if (cmd.command.includes('CTL') || cmd.command.includes('LIN')) {
    categories['通信管理'].push(cmd);
  } else if (cmd.command.includes('SPL')) {
    categories['スプール管理'].push(cmd);
  } else if (cmd.command.includes('OUTQ')) {
    categories['出力待ち行列管理'].push(cmd);
  } else if (cmd.command.includes('WTR')) {
    categories['書出しプログラム管理'].push(cmd);
  } else if (cmd.command.includes('SAV') || cmd.command.includes('RST')) {
    categories['保管・復元'].push(cmd);
  } else {
    categories['その他のユーティリティ'].push(cmd);
  }
});

// カテゴリ別に出力
Object.keys(categories).forEach((category, index) => {
  if (categories[category].length > 0) {
    markdown += `## ${index + 1}. ${category}\n\n`;
    markdown += '| コマンド名 | 機能説明 |\n';
    markdown += '|------------|----------|\n';
    
    categories[category].sort((a, b) => a.command.localeCompare(b.command));
    categories[category].forEach(cmd => {
      markdown += `| ${cmd.command} | ${cmd.description} |\n`;
    });
    markdown += '\n';
  }
});

// 実装優先度を追加
markdown += '## 実装優先度\n\n';
markdown += '### 高優先度（基本機能）\n';
markdown += '1. **ジョブ管理**: SBMJOB, ENDJOB, WRKJOB, WRKACTJOB\n';
markdown += '2. **ライブラリ管理**: CRTLIB, DLTLIB, WRKLIB, DSPLIBL\n';
markdown += '3. **ファイル管理**: CRTPF, CRTLF, CPYF, DSPFD\n';
markdown += '4. **プログラム管理**: CRTCBLPGM, CRTPGM, CALL\n';
markdown += '5. **セキュリティ**: CRTUSRPRF, CHGUSRPRF, GRTOBJAUT\n\n';

markdown += '### 中優先度（開発支援）\n';
markdown += '1. **ソース管理**: STRSEU, WRKMBRPDM\n';
markdown += '2. **デバッグ**: STRDBG, DSPJOBLOG\n';
markdown += '3. **スプール管理**: WRKSPLF, DSPSPLF\n';
markdown += '4. **メッセージ管理**: SNDMSG, DSPMSG\n\n';

markdown += '### 低優先度（拡張機能）\n';
markdown += '1. **通信管理**: CRTCTLD, CRTLIND\n';
markdown += '2. **保管・復元**: SAVLIB, RSTLIB\n';
markdown += '3. **装置管理**: CRTDEVDSP, CRTDEVPRT\n\n';

markdown += '## 実装方針\n\n';
markdown += '1. **コマンドインターフェース**: Web UIとCLIの両方で提供\n';
markdown += '2. **互換性**: ASPシステムとの最大限の互換性を確保\n';
markdown += '3. **拡張性**: OpenASP独自の拡張機能も追加可能\n';
markdown += '4. **セキュリティ**: モダンなセキュリティ標準に準拠\n';
markdown += '5. **パフォーマンス**: クラウド環境に最適化\n\n';

markdown += '## 注意事項\n\n';
markdown += '- 一部のコマンドはOpenASP環境の制約により動作が異なる場合があります\n';
markdown += '- 外部媒体関連のコマンドはクラウド環境に合わせて実装方法を変更します\n';
markdown += '- セキュリティ関連コマンドは現代的な認証・認可方式に対応します\n';

// ファイルに保存
fs.writeFileSync('/data/asp_system_cmd.md', markdown, 'utf8');
console.log('Markdownファイルを /data/asp_system_cmd.md に保存しました。');
console.log(`総コマンド数: ${commands.length}`);

// カテゴリ別の集計を表示
console.log('\nカテゴリ別コマンド数:');
Object.keys(categories).forEach(category => {
  if (categories[category].length > 0) {
    console.log(`${category}: ${categories[category].length}`);
  }
});