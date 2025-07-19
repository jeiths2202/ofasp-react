import React, { useState, useEffect, useRef, useCallback } from 'react';
import './AspCliWebTerminal.css';

interface AspCliWebTerminalProps {
  isDarkMode: boolean;
}

interface CommandHistory {
  command: string;
  output: string;
  timestamp: Date;
  success: boolean;
}

interface SystemInfo {
  currentUser: string;
  currentVolume: string;
  currentLibrary: string;
  systemTime: string;
}

const AspCliWebTerminal: React.FC<AspCliWebTerminalProps> = ({ isDarkMode }) => {
  const [commandHistory, setCommandHistory] = useState<CommandHistory[]>([]);
  const [currentCommand, setCurrentCommand] = useState('');
  const [isExecuting, setIsExecuting] = useState(false);
  const [historyIndex, setHistoryIndex] = useState(-1);
  const [systemInfo, setSystemInfo] = useState<SystemInfo>({
    currentUser: 'ASPUSER',
    currentVolume: 'DISK99',
    currentLibrary: 'QGPL',
    systemTime: new Date().toLocaleString()
  });
  
  const terminalRef = useRef<HTMLDivElement>(null);
  const inputRef = useRef<HTMLInputElement>(null);
  const [commandSuggestions] = useState([
    'CRTLIB', 'DLTLIB', 'WRKLIB', 'CRTFILE', 'DLTFILE', 
    'DSPFD', 'WRKOBJ', 'WRKVOL', 'WRKSPLF', 'WRKMSG',
    'DSPJOB', 'SAVLIB', 'RSTLIB', 'SNDMSG', 'RCVMSG'
  ]);

  // 시스템 시간 업데이트
  useEffect(() => {
    const timer = setInterval(() => {
      setSystemInfo(prev => ({
        ...prev,
        systemTime: new Date().toLocaleString()
      }));
    }, 1000);

    return () => clearInterval(timer);
  }, []);

  // 터미널 스크롤 자동 조정
  useEffect(() => {
    if (terminalRef.current) {
      terminalRef.current.scrollTop = terminalRef.current.scrollHeight;
    }
  }, [commandHistory]);

  // ASP 명령어 실행
  const executeCommand = useCallback(async (command: string) => {
    if (!command.trim()) return;

    setIsExecuting(true);
    const timestamp = new Date();
    let commandOutput = '';
    
    try {
      // Python aspcli.py 호출
      const response = await fetch('/api/asp-command', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ 
          command: command.trim(),
          user: systemInfo.currentUser 
        }),
      });

      let output = '';
      let success = false;

      if (response.ok) {
        const result = await response.json();
        output = result.output || result.error || '명령이 실행되었습니다.';
        success = result.success !== false;
      } else {
        // 백엔드가 없는 경우 시뮬레이션
        output = await simulateCommand(command.trim());
        success = true;
      }

      const newEntry: CommandHistory = {
        command: command.trim(),
        output,
        timestamp,
        success
      };

      commandOutput = output;
      setCommandHistory(prev => [...prev, newEntry]);
      
    } catch (error) {
      // 에러 발생 시 시뮬레이션으로 폴백
      const output = await simulateCommand(command.trim());
      const newEntry: CommandHistory = {
        command: command.trim(),
        output,
        timestamp,
        success: true
      };
      commandOutput = output;
      setCommandHistory(prev => [...prev, newEntry]);
    }

    setIsExecuting(false);
    setCurrentCommand('');
    setHistoryIndex(-1);
    
    // 커서 위치 설정
    setTimeout(() => {
      focusCursor(commandOutput);
    }, 100);
  }, [systemInfo.currentUser]);

  // 명령어 시뮬레이션 (백엔드 없을 때)
  const simulateCommand = async (command: string): Promise<string> => {
    const upperCommand = command.toUpperCase();
    
    // 명령어 파싱
    const parts = upperCommand.split(' ');
    const mainCommand = parts[0];
    
    await new Promise(resolve => setTimeout(resolve, 500)); // 시뮬레이션 지연

    switch (mainCommand) {
      case 'WRKVOL':
        return `[INFO] ボリューム状況:
  📦 ボリューム名      : DISK99
     ├ ライブラリ数   : 3
     ├ 総ファイル数   : 12
     └ ディスク使用量 : 1,024 Byte
  📦 ボリューム名      : MSGQ
     ├ ライブラリ数   : 1
     ├ 総ファイル数   : 0
     └ ディスク使用量 : 0 Byte`;

      case 'CRTLIB':
        const libMatch = command.match(/LIB-(\w+)/i);
        const volMatch = command.match(/VOL-(\w+)/i);
        const libName = libMatch ? libMatch[1] : 'NEWLIB';
        const volName = volMatch ? volMatch[1] : 'DISK99';
        return `[INFO] ライブラリ '${libName}' がボリューム '${volName}' に作成されました。`;

      case 'CRTFILE':
        const fileMatch = command.match(/FILE\((\w+)\/(\w+)\)/i);
        if (fileMatch) {
          const [, lib, file] = fileMatch;
          return `[INFO] ファイル '${file}' がライブラリ '${lib}' に作成されました。`;
        }
        return `[INFO] ファイルが作成されました。`;

      case 'DSPFD':
        const dspfdMatch = command.match(/FILE\((\w+)\/(\w+)\)/i);
        if (dspfdMatch) {
          const [, lib, file] = dspfdMatch;
          return `[INFO] ファイル定義情報:
  📁 ファイルパス     : /volume/DISK99/${lib}/${file}
  📄 ファイル名       : ${file}
  📦 ファイルサイズ   : 0 Byte
  🕒 作成日時         : ${new Date().toLocaleString()}
  📉 ファイルが空です : はい`;
        }
        return `[INFO] ファイル定義情報を表示します。`;

      case 'HELP':
        const parts = command.split(' ');
        if (parts.length > 1) {
          const helpCommand = parts[1].toUpperCase();
          return getCommandHelp(helpCommand);
        }
        return `使用可能なASPコマンド:

ライブラリ管理:
  CRTLIB LIB-<name>,VOL-<volume>  - ライブラリ作成
  DLTLIB LIB-<name>,VOL-<volume>  - ライブラリ削除
  WRKLIB                          - ライブラリ一覧

ファイル管理:
  CRTFILE FILE(<lib>/<file>),VOL-<volume>  - ファイル作成
  DLTFILE FILE(<lib>/<file>),VOL-<volume>  - ファイル削除
  DSPFD FILE(<lib>/<file>),VOL-<volume>    - ファイル定義表示

システム管理:
  WRKVOL     - ボリューム状況
  WRKOBJ LIB-<name>,VOL-<volume>  - オブジェクト作業
  WRKSPLF    - スプールファイル作業
  DSPJOB     - ジョブ表示

メッセージ管理:
  SNDMSG TO-<user>,MSG-<message>  - メッセージ送信
  RCVMSG USER-<user>              - メッセージ受信
  WRKMSG                          - メッセージキュー表示

プログラム実行:
  CALL PGM-<lib>/<prog>,VOL-<volume>  - プログラム実行

バックアップ:
  SAVLIB LIB-<name>,VOL-<volume>  - ライブラリ保存
  RSTLIB FILE-<backup_file>       - ライブラリ復元

詳細なヘルプ: HELP <コマンド名>
例: HELP CRTLIB, HELP WRKVOL`;

      case 'CLS':
      case 'CLEAR':
        setCommandHistory([]);
        return '';

      default:
        if (upperCommand.includes('LIB-') || upperCommand.includes('FILE(') || upperCommand.includes('VOL-')) {
          return `[INFO] コマンド '${mainCommand}' が実行されました。`;
        }
        return `[ERROR] 不明なコマンドです: ${mainCommand}
HELP を入力して使用可能なコマンドを確認してください。`;
    }
  };

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    if (currentCommand.trim() && !isExecuting) {
      executeCommand(currentCommand);
    }
  };

  // 各コマンドの詳細ヘルプ
  const getCommandHelp = (command: string): string => {
    switch (command) {
      case 'CRTLIB':
        return `CRTLIB - ライブラリ作成

概要:
  新しいライブラリを指定されたボリュームに作成します。

構文:
  CRTLIB LIB-<library_name>,VOL-<volume_name>

パラメータ:
  LIB-<library_name>  : 作成するライブラリ名
  VOL-<volume_name>    : 作成先ボリューム名

例:
  CRTLIB LIB-TESTLIB,VOL-DISK99
  CRTLIB LIB-SALES,VOL-DISK01

注意:
  - ライブラリ名は英数字のみ使用可能
  - 同名のライブラリが存在する場合は上書きされます`;

      case 'DLTLIB':
        return `DLTLIB - ライブラリ削除

概要:
  指定されたライブラリとその中身を完全に削除します。

構文:
  DLTLIB LIB-<library_name>,VOL-<volume_name>

パラメータ:
  LIB-<library_name>  : 削除するライブラリ名
  VOL-<volume_name>    : 対象ボリューム名

例:
  DLTLIB LIB-TESTLIB,VOL-DISK99
  DLTLIB LIB-OLDLIB,VOL-DISK01

警告:
  - この操作は元に戻せません
  - ライブラリ内の全ファイルも削除されます`;

      case 'WRKVOL':
        return `WRKVOL - ボリューム状況表示

概要:
  システム内の全ボリュームの使用状況を表示します。

構文:
  WRKVOL

表示情報:
  - ボリューム名
  - ライブラリ数
  - 総ファイル数
  - ディスク使用量

例:
  WRKVOL

出力例:
  📦 ボリューム名      : DISK99
     ├ ライブラリ数   : 3
     ├ 総ファイル数   : 12
     └ ディスク使用量 : 1,024 Byte`;

      case 'CRTFILE':
        return `CRTFILE - ファイル作成

概要:
  指定されたライブラリ内にファイルを作成します。

構文:
  CRTFILE FILE(<library>/<filename>),VOL-<volume>

パラメータ:
  FILE(<library>/<filename>) : ライブラリ/ファイル名
  VOL-<volume>               : 対象ボリューム名

例:
  CRTFILE FILE(TESTLIB/CUSTMAST),VOL-DISK99
  CRTFILE FILE(SALES/REPORT),VOL-DISK01

注意:
  - 指定されたライブラリが存在する必要があります
  - ファイルは空の状態で作成されます`;

      case 'DSPFD':
        return `DSPFD - ファイル定義表示

概要:
  指定されたファイルの詳細情報を表示します。

構文:
  DSPFD FILE(<library>/<filename>),VOL-<volume>

パラメータ:
  FILE(<library>/<filename>) : ライブラリ/ファイル名
  VOL-<volume>               : 対象ボリューム名

表示情報:
  - ファイルパス
  - ファイルサイズ
  - 作成日時
  - 最終更新日時
  - 空ファイル判定

例:
  DSPFD FILE(TESTLIB/CUSTMAST),VOL-DISK99`;

      case 'WRKOBJ':
        return `WRKOBJ - オブジェクト作業

概要:
  指定されたライブラリ内のオブジェクト一覧を表示します。

構文:
  WRKOBJ LIB-<library_name>,VOL-<volume_name>

パラメータ:
  LIB-<library_name>  : 対象ライブラリ名
  VOL-<volume_name>    : 対象ボリューム名

表示情報:
  - オブジェクト名
  - ファイルサイズ
  - 最終更新日時

例:
  WRKOBJ LIB-TESTLIB,VOL-DISK99`;

      case 'SNDMSG':
        return `SNDMSG - メッセージ送信

概要:
  指定されたユーザーにメッセージを送信します。

構文:
  SNDMSG TO-<username>,MSG-<message>

パラメータ:
  TO-<username>  : 送信先ユーザー名
  MSG-<message>  : 送信メッセージ内容

例:
  SNDMSG TO-ADMIN,MSG-システム開始しました
  SNDMSG TO-USER01,MSG-処理完了

注意:
  - メッセージは日時付きで保存されます`;

      case 'RCVMSG':
        return `RCVMSG - メッセージ受信

概要:
  指定されたユーザーの受信メッセージを表示します。

構文:
  RCVMSG USER-<username>

パラメータ:
  USER-<username>  : 対象ユーザー名

例:
  RCVMSG USER-ADMIN
  RCVMSG USER-USER01

注意:
  - 受信したメッセージがない場合は通知されます`;

      case 'DSPJOB':
        return `DSPJOB - ジョブ表示

概要:
  システム内のジョブ実行履歴を表示します。

構文:
  DSPJOB

表示情報:
  - ジョブID
  - プログラム名
  - 開始時刻
  - 終了時刻
  - 実行ステータス

例:
  DSPJOB

注意:
  - 最新10件のジョブ履歴が表示されます`;

      case 'CALL':
        return `CALL - プログラム実行

概要:
  指定されたプログラムを実行します。

構文:
  CALL PGM-<library>/<program>,VOL-<volume>

パラメータ:
  PGM-<library>/<program>  : ライブラリ/プログラム名
  VOL-<volume>             : 対象ボリューム名

対応形式:
  - Python (.py)
  - Shell Script (.sh)

例:
  CALL PGM-TESTLIB/HELLO,VOL-DISK99
  CALL PGM-BATCH/PROCESS,VOL-DISK01`;

      case 'SAVLIB':
        return `SAVLIB - ライブラリ保存

概要:
  指定されたライブラリをバックアップファイルに保存します。

構文:
  SAVLIB LIB-<library_name>,VOL-<volume_name>

パラメータ:
  LIB-<library_name>  : 保存するライブラリ名
  VOL-<volume_name>    : 対象ボリューム名

例:
  SAVLIB LIB-TESTLIB,VOL-DISK99

注意:
  - バックアップファイルは自動的に日時付きで命名されます
  - tar.gz形式で圧縮保存されます`;

      case 'RSTLIB':
        return `RSTLIB - ライブラリ復元

概要:
  バックアップファイルからライブラリを復元します。

構文:
  RSTLIB FILE-<backup_filename>

パラメータ:
  FILE-<backup_filename>  : 復元するバックアップファイル名

例:
  RSTLIB FILE-TESTLIB_DISK99_20250719120000.tar.gz

注意:
  - バックアップファイルが存在する必要があります
  - 既存の同名ライブラリは上書きされます`;

      case 'WRKLIB':
        return `WRKLIB - ライブラリ一覧

概要:
  システム内の全ライブラリを一覧表示します。

構文:
  WRKLIB

表示情報:
  - ボリューム名
  - ライブラリ名

例:
  WRKLIB`;

      case 'WRKSPLF':
        return `WRKSPLF - スプールファイル作業

概要:
  システム内のスプールファイルを一覧表示します。

構文:
  WRKSPLF

表示情報:
  - スプールファイル名
  - ファイルサイズ
  - 最終更新日時

例:
  WRKSPLF`;

      case 'WRKMSG':
        return `WRKMSG - メッセージキュー表示

概要:
  システムメッセージキューの内容を表示します。

構文:
  WRKMSG

例:
  WRKMSG

注意:
  - システム全体のメッセージログが表示されます`;

      default:
        return `[ERROR] 不明なコマンドです: ${command}
使用可能なコマンド一覧を見るには HELP と入力してください。`;
    }
  };

  // 커서 포커스 관리
  const focusCursor = (output: string) => {
    // SMED 맵 출력에서 입력 필드가 있는지 확인
    if (output.includes('입력필드') || output.includes('INPUT') || output.includes('_____')) {
      // SMED 맵 입력 필드가 있으면 해당 필드에 포커스
      const mapInputField = document.querySelector('.map-input-field') as HTMLInputElement;
      if (mapInputField) {
        mapInputField.focus();
        return;
      }
    }
    
    // 그 외의 경우는 명령어 입력창에 포커스
    if (inputRef.current) {
      inputRef.current.focus();
    }
  };

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === 'Tab') {
      e.preventDefault();
      // 명령어 자동완성
      const suggestions = commandSuggestions.filter(cmd => 
        cmd.toLowerCase().startsWith(currentCommand.toLowerCase())
      );
      if (suggestions.length === 1) {
        setCurrentCommand(suggestions[0] + ' ');
      }
    } else if (e.key === 'ArrowUp') {
      e.preventDefault();
      // 이전 명령어 불러오기 (최대 10개)
      const maxHistory = Math.min(commandHistory.length, 10);
      const recentHistory = commandHistory.slice(-maxHistory);
      
      if (recentHistory.length > 0) {
        const newIndex = historyIndex === -1 ? recentHistory.length - 1 : Math.max(0, historyIndex - 1);
        setHistoryIndex(newIndex);
        setCurrentCommand(recentHistory[newIndex].command);
      }
    } else if (e.key === 'ArrowDown') {
      e.preventDefault();
      // 다음 명령어 불러오기
      const maxHistory = Math.min(commandHistory.length, 10);
      const recentHistory = commandHistory.slice(-maxHistory);
      
      if (historyIndex >= 0) {
        if (historyIndex < recentHistory.length - 1) {
          const newIndex = historyIndex + 1;
          setHistoryIndex(newIndex);
          setCurrentCommand(recentHistory[newIndex].command);
        } else {
          setHistoryIndex(-1);
          setCurrentCommand('');
        }
      }
    }
  };

  const clearHistory = () => {
    setCommandHistory([]);
    setHistoryIndex(-1);
  };

  return (
    <div className={`asp-cli-terminal ${isDarkMode ? 'dark' : 'light'}`}>
      {/* 헤더 */}
      <div className="terminal-header">
        <div className="header-title">
          <span className="title-icon">🖥️</span>
          <span className="title-text">ASP System Command Terminal</span>
        </div>
        <div className="header-info">
          <span className="info-item">ユーザー: {systemInfo.currentUser}</span>
          <span className="info-separator">|</span>
          <span className="info-item">ボリューム: {systemInfo.currentVolume}</span>
          <span className="info-separator">|</span>
          <span className="info-item">{systemInfo.systemTime}</span>
        </div>
        <button 
          className="clear-button"
          onClick={clearHistory}
          title="画面クリア"
        >
          🗑️
        </button>
      </div>

      {/* 터미널 본문 */}
      <div className="terminal-body" ref={terminalRef}>
        {/* 시작 메시지 */}
        {commandHistory.length === 0 && (
          <div className="welcome-message">
            <div className="welcome-logo">
              ╔══════════════════════════════════════════╗
              ║           ASP System Command             ║
              ║              Terminal v2.0               ║
              ╚══════════════════════════════════════════╝
            </div>
            <div className="welcome-text">
              ASP システムコマンドターミナルへようこそ。<br/>
              ヘルプを表示するには <strong>HELP</strong> を入力してください。
            </div>
          </div>
        )}

        {/* 명령어 히스토리 */}
        {commandHistory.map((entry, index) => (
          <div key={index} className="command-entry">
            <div className="command-line">
              <span className="prompt">ASP&gt;</span>
              <span className="command-text">{entry.command}</span>
              <span className="command-time">
                [{entry.timestamp.toLocaleTimeString()}]
              </span>
            </div>
            <div className={`command-output ${entry.success ? 'success' : 'error'}`}>
              <pre>{entry.output}</pre>
            </div>
          </div>
        ))}

        {/* 실행 중 표시 */}
        {isExecuting && (
          <div className="executing-indicator">
            <span className="prompt">ASP&gt;</span>
            <span className="command-text">{currentCommand}</span>
            <span className="loading-dots">実行中...</span>
          </div>
        )}
      </div>

      {/* 명령어 입력 */}
      <form className="terminal-input" onSubmit={handleSubmit}>
        <span className="input-prompt">ASP&gt;</span>
        <input
          ref={inputRef}
          type="text"
          value={currentCommand}
          onChange={(e) => setCurrentCommand(e.target.value)}
          onKeyDown={handleKeyDown}
          placeholder="ASP コマンドを入力してください... (例: WRKVOL, HELP)"
          disabled={isExecuting}
          className="command-input"
          autoFocus
        />
        <button 
          type="submit" 
          disabled={isExecuting || !currentCommand.trim()}
          className="execute-button"
        >
          {isExecuting ? '実行中...' : '実行'}
        </button>
      </form>

      {/* 도움말 패널 */}
      <div className="help-panel">
        <strong>ショートカット:</strong> Tab(自動完成), ↑↓(コマンド履歴), Ctrl+L(画面クリア) | <strong>履歴:</strong> 最大 10 コマンド保存
      </div>
    </div>
  );
};

export default AspCliWebTerminal;