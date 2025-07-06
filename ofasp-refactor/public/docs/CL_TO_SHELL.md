# CL TO SHELL 変換ツール

CL (Control Language) プログラムを Shell Script に変換するツールです。

## 概要

CLプログラムをUnix/Linux ShellスクリプトやWindowsバッチファイルに変換します。

## 特徴

- クロスプラットフォーム: Windows、Linux、macOSで実行
- システム管理: ジョブ制御、プロセス管理
- ファイル操作: コピー、移動、削除、権限設定
- ネットワーク: リモート実行、ファイル転送

## 使用方法

```bash
node /data/cl-converter.js input.cl output.sh --target=shell
```

## 変換例

**CL:**
```cl
PGM
  DCL VAR(&FILENAME) TYPE(*CHAR) LEN(50)
  CHGVAR VAR(&FILENAME) VALUE('TESTFILE')
  CPYF FROMFILE(&FILENAME) TOFILE(BACKUP)
  SNDPGMMSG MSG('Backup completed')
ENDPGM
```

**Shell:**
```bash
#!/bin/bash
FILENAME="TESTFILE"
cp "$FILENAME" "BACKUP"
echo "Backup completed"
```

## 対応コマンド

### ファイル操作
- `CPYF` → `cp` (ファイルコピー)
- `DLTF` → `rm` (ファイル削除)
- `RNMOBJ` → `mv` (ファイル名変更)
- `CRTPF` → `touch` (ファイル作成)

### ジョブ制御
- `SBMJOB` → バックグラウンド実行
- `DLYJOB` → `sleep` (待機)
- `ENDJOB` → `kill` (ジョブ終了)

### システム情報
- `DSPSYSSTS` → システム状態表示
- `DSPJOB` → `ps` (プロセス表示)
- `WRKJOB` → ジョブ操作

*詳細なドキュメントは準備中です。*

---
**OpenASP Project Team - 2025**