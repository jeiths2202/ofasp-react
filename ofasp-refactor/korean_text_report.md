# Korean Text Found in Source Files - Conversion Report

This report lists all Korean text found in the source files that should be converted to Japanese.

## Summary
- Total files with Korean text: 15 files
- Excluded: i18n/ko.json (Korean language support file)

## Files with Korean Text

### 1. `/src/App.tsx`
- **Line 206**: Comment - `// 테마 및 언어 초기화` → Should be `// テーマおよび言語の初期化`
- **Line 222**: Comment - `// 환경변수 또는 기본값으로 일본어 설정` → Should be `// 環境変数またはデフォルト値で日本語設定`
- **Line 228**: Comment - `// 언어 변경 시 탭 업데이트` → Should be `// 言語変更時のタブ更新`
- **Line 232**: Comment - `// 초기 대시보드 탭 생성 또는 업데이트` → Should be `// 初期ダッシュボードタブ生成または更新`
- **Line 244**: Comment - `// 기존 대시보드 탭 업데이트` → Should be `// 既存のダッシュボードタブ更新`
- **Line 249**: Comment - `// 새 대시보드 탭 추가` → Should be `// 新しいダッシュボードタブ追加`

### 2. `/src/components/Sidebar.tsx`
- **Line 241**: Comment - `// 언어 선택기` → Should be `// 言語選択器`

### 3. `/src/pages/SourceConversionPage.tsx`
- **Line 317**: String - `' (SOSI: SO - 더블바이트 시작)'` → Should be `' (SOSI: SO - ダブルバイト開始)'`
- **Line 318**: String - `' (SOSI: SI - 더블바이트 종료)'` → Should be `' (SOSI: SI - ダブルバイト終了)'`
- **Line 319**: String - `' (EBCDIC 공백)'` → Should be `' (EBCDIC 空白)'`
- **Line 658**: UI Text - `Remove SOSI codes (出力에서 제거)` → Should be `Remove SOSI codes (出力から削除)`
- **Line 671**: UI Text - `Keep SOSI codes (0x0E/0x0F 유지)` → Should be `Keep SOSI codes (0x0E/0x0F 維持)`
- **Line 684**: UI Text - `Convert to spaces (0x20으로 변환)` → Should be `Convert to spaces (0x20に変換)`

### 4. `/src/pages/DocumentationPage.tsx`
- **Line 174**: Comment - `// 파일이 없는 경우 기본 메시지 표시` → Should be `// ファイルがない場合、デフォルトメッセージ表示`
- **Line 175**: String - `이 섹션의 문서는 준비 중입니다.` → Should be `このセクションのドキュメントは準備中です。`
- **Line 216**: Comment - `// フォールバック: ファイル読み込み失敗時 기본 내용 반환` → Should be `// フォールバック: ファイル読み込み失敗時デフォルト内容返還`

### 5. `/src/pages/AITransformPage.tsx`
- **Line 154**: UI Text - `AI 분석 진행중` → Should be `AI 分析進行中`
- **Line 159**: UI Text - `AI 신경망이 CL, COBOL, COPYBOOK 파일들의 의존성을 분석하고 있습니다...` → Should be `AI ニューラルネットワークがCL、COBOL、COPYBOOKファイルの依存関係を分析しています...`
- **Line 368**: Alert - `'디렉토리 경로를 입력해주세요.'` → Should be `'ディレクトリパスを入力してください。'`
- **Line 404**: Alert - `${newFiles.length}개의 파일을 불러왔습니다.` → Should be `${newFiles.length}個のファイルを読み込みました。`
- **Line 406**: Alert - `'해당 경로에서 지원되는 파일을 찾을 수 없습니다.'` → Should be `'該当パスでサポートされるファイルが見つかりません。'`
- **Line 410**: Alert - `디렉토리 스캔 중 오류가 발생했습니다:` → Should be `ディレクトリスキャン中にエラーが発生しました:`
- **Line 598**: Alert - `'분석할 파일을 선택해주세요.'` → Should be `'分析するファイルを選択してください。'`
- **Line 732**: Comment - `// 분석 결과 초기화` → Should be `// 分析結果初期化`
- **Line 740**: Comment - `// 파일들의 변환 상태 초기화` → Should be `// ファイルの変換状態初期化`
- **Line 798**: UI Text - `'드롭하여 업로드'` → Should be `'ドロップしてアップロード'`
- **Line 798**: UI Text - `'파일 또는 폴더를 드래그하거나 선택하세요'` → Should be `'ファイルまたはフォルダをドラッグまたは選択してください'`
- **Line 801**: UI Text - `COBOL, COPYBOOK, CL, SMED 파일을 지원합니다` → Should be `COBOL、COPYBOOK、CL、SMEDファイルをサポートします`
- **Line 828**: UI Text - `파일 선택` → Should be `ファイル選択`
- **Line 836**: UI Text - `폴더 선택` → Should be `フォルダ選択`
- **Line 843**: UI Text - `또는 서버 디렉토리 경로를 직접 입력하세요:` → Should be `またはサーバーディレクトリパスを直接入力してください:`
- **Line 851**: Placeholder - `"예: /data/assets/SRC1.COBLIB"` → Should be `"例: /data/assets/SRC1.COBLIB"`
- **Line 864**: Button - `'스캔 중...'` / `'스캔'` → Should be `'スキャン中...'` / `'スキャン'`
- **Line 870**: UI Text - `빠른 선택:` → Should be `クイック選択:`
- **Line 895**: UI Text - `파일을 놓아주세요` → Should be `ファイルをドロップしてください`
- **Line 904**: UI Text - `총 ${files.length}개 파일이 업로드되었습니다.` → Should be `合計 ${files.length}個のファイルがアップロードされました。`
- **Line 920**: Button - `'분석 중...'` / `'분석하기'` → Should be `'分析中...'` / `'分析する'`
- **Line 937**: Button - `분석정보 초기화` → Should be `分析情報初期化`
- **Line 1092**: UI Text - `프로그램 분석 결과` → Should be `プログラム分析結果`
- **Line 1098**: UI Text - `📊 분석 요약` → Should be `📊 分析要約`
- **Line 1104-1116**: UI Labels:
  - `전체 파일` → `全体ファイル`
  - `CL 파일` → `CLファイル`
  - `프로그램 호출` → `プログラム呼び出し`
  - `누락 프로그램` → `欠落プログラム`
- **Line 1126**: UI Text - `📜 CL 프로그램 목록` → Should be `📜 CLプログラムリスト`
- **Line 1147**: UI Text - `${clProgram.calls.length}개 호출, ${clProgram.libraries.length}개 라이브러리` → Should be `${clProgram.calls.length}個の呼び出し、${clProgram.libraries.length}個のライブラリ`
- **Line 1158**: UI Text - `CL 파일이 없습니다` → Should be `CLファイルがありません`
- **Line 1167**: UI Text - `🌳 호출 트리 (Call Tree)` → Should be `🌳 呼び出しツリー (Call Tree)`
- **Line 1178**: UI Text - `(R) = 반복 호출 (NOF) = 찾을 수 없는 프로그램` → Should be `(R) = 繰り返し呼び出し (NOF) = 見つからないプログラム`
- **Line 1183**: UI Text - `CL 프로그램을 선택하면 호출 트리가 표시됩니다` → Should be `CLプログラムを選択すると呼び出しツリーが表示されます`

### 6. `/src/pages/ToolsPage.tsx`
- **Lines 69-122**: Console log messages:
  - `=== 변환 시작 ===` → `=== 変換開始 ===`
  - `방향:` → `方向:`
  - `인코딩:` → `エンコーディング:`
  - `입력 데이터:` → `入力データ:`
  - `입력 길이:` → `入力長:`
  - `SOSI 사용:` → `SOSI使用:`
  - `SOSI 타입:` → `SOSIタイプ:`
  - `SOSI 처리:` → `SOSI処理:`
  - `오류 처리:` → `エラー処理:`
  - `ASCII 출력:` → `ASCII出力:`
  - `EBCDIC 출력:` → `EBCDIC出力:`
  - `출력 길이:` → `出力長:`
  - `경고:` → `警告:`
  - `변환 완료 (오류 없음)` → `変換完了 (エラーなし)`
  - `=== 변환 종료 ===` → `=== 変換終了 ===`
  - `변환 실패:` → `変換失敗:`
- **Line 143**: Alert - `'저장할 EBCDIC 데이터가 없습니다.'` → Should be `'保存するEBCDICデータがありません。'`
- **Line 165**: Alert - `'파일 저장 중 오류가 발생했습니다.'` → Should be `'ファイル保存中にエラーが発生しました。'`
- **Line 171**: Alert - `'저장할 ASCII 데이터가 없습니다.'` → Should be `'保存するASCIIデータがありません。'`
- **Line 406**: UI Text - Same as SourceConversionPage line 658
- **Line 419**: UI Text - Same as SourceConversionPage line 671
- **Line 432**: UI Text - Same as SourceConversionPage line 684
- **Line 449**: Console log - `'데이터 초기화 완료'` → Should be `'データ初期化完了'`
- **Line 569**: Console log - `'디버그 콘솔 활성화'` → Should be `'デバッグコンソール有効化'`

### 7. `/src/pages/ClAXPage.tsx`
- **Line 35**: Comment - `// 실제 refactoring API 호출이 들어갈 부분` → Should be `// 実際のrefactoring API呼び出しが入る部分`
- **Line 82**: Comment - `// 새 파일 선택 시 이전 변환 결과 초기화` → Should be `// 新しいファイル選択時、以前の変換結果を初期化`
- **Line 88**: Comment - `// 파일 선택 후 input을 초기화하여 동일 파일 재선택 가능하도록 함` → Should be `// ファイル選択後、inputを初期化して同じファイルを再選択可能にする`
- **Line 95**: Comment - `// 파일 input을 강제로 초기화한 후 클릭` → Should be `// ファイルinputを強制的に初期化した後クリック`
- **Line 113**: Comment - `// 설정 패널` → Should be `// 設定パネル`
- **Line 174**: Comment - `// 코드 편집 영역` → Should be `// コード編集エリア`
- **Line 176**: Comment - `// 소스 코드` → Should be `// ソースコード`
- **Line 207**: Comment - `// 리팩토링된 코드` → Should be `// リファクタリングされたコード`
- **Line 242**: Comment - `// CL 명령어 참조` → Should be `// CLコマンド参照`

### 8. `/src/pages/CobolAXPage.tsx`
- **Line 36**: Comment - `// Enhanced COBOL 변환 로직` → Should be `// Enhanced COBOL変換ロジック`
- **Line 41**: Comment - `// Java 변환의 경우 Pure Converter 사용 (sampleData 없이 실제 파싱만)` → Should be `// Java変換の場合、Pure Converterを使用 (sampleDataなしで実際のパーシングのみ)`
- **Line 48**: Comment - `// EMPPAY01.cob 또는 EMPPAY02.cob인 경우 특별한 Java 변환 적용 (fallback)` → Should be `// EMPPAY01.cobまたはEMPPAY02.cobの場合、特別なJava変換を適用 (fallback)`
- **Line 59**: Comment - `// 기본 변환 로직` → Should be `// デフォルト変換ロジック`
- **Line 227**: Comment - `// 간단한 COBOL 변환 로직 적용` → Should be `// 簡単なCOBOL変換ロジックを適用`
- **Line 243**: Comment - `// 새 파일 선택 시 이전 변환 결과 초기화` → Should be `// 新しいファイル選択時、以前の変換結果を初期化`
- **Line 249**: Comment - `// 파일 선택 후 input을 초기화하여 동일 파일 재선택 가능하도록 함` → Should be `// ファイル選択後、inputを初期化して同じファイルを再選択可能にする`
- **Line 256**: Comment - `// 파일 input을 강제로 초기화한 후 클릭` → Should be `// ファイルinputを強制的に初期化した後クリック`
- **Line 281**: Comment - `// 샘플 로드 시 이전 변환 결과 초기화` → Should be `// サンプルロード時、以前の変換結果を初期化`
- **Line 286**: Comment - `// /data/EMPPAY01.cob의 내용을 로드` → Should be `// /data/EMPPAY01.cobの内容をロード`
- **Line 370**: Comment - `// EMPPAY01 로드 시 이전 변환 결과 초기화` → Should be `// EMPPAY01ロード時、以前の変換結果を初期化`
- **Line 375**: Comment - `// /data/EMPPAY02.cob의 내용을 로드` → Should be `// /data/EMPPAY02.cobの内容をロード`
- **Line 440**: Comment - `// EMPPAY02 로드 시 이전 변환 결과 초기화` → Should be `// EMPPAY02ロード時、以前の変換結果を初期化`
- **Line 444**: Comment - `// COBOL 소스에서 currency 형식 추출` → Should be `// COBOLソースからcurrency形式を抽出`
- **Line 446**: Comment - `// PIC 절에서 currency 패턴 찾기 - 달러와 엔화 모두 지원` → Should be `// PIC節でcurrencyパターンを探す - ドルと円の両方をサポート`
- **Line 461**: Comment - `// 기본값은 달러` → Should be `// デフォルトはドル`
- **Line 467**: Comment - `// 실제 COBOL 소스에서 currency 형식 추출` → Should be `// 実際のCOBOLソースからcurrency形式を抽出`
- **Line 631**: Comment - `// 실제 Java 컴파일 및 실행 시뮬레이션` → Should be `// 実際のJavaコンパイルおよび実行シミュレーション`
- **Line 637**: Comment - `// 각 샘플별 실제 실행 결과 시뮬레이션` → Should be `// 各サンプル別の実際の実行結果シミュレーション`
- **Line 877**: Comment - `// EMPPAY01, EMPPAY02의 경우 기존 급여 시스템 출력 사용` → Should be `// EMPPAY01、EMPPAY02の場合、既存の給与システム出力を使用`
- **Line 932**: Comment - `// 설정 패널` → Should be `// 設定パネル`
- **Line 949**: Comment - `// 대상 언어 변경 시 모든 상태 초기화` → Should be `// ターゲット言語変更時、すべての状態を初期化`
- **Line 1004**: Comment - `// 코드 편집 영역` → Should be `// コード編集エリア`
- **Line 1006**: Comment - `// 소스 코드` → Should be `// ソースコード`
- **Line 1037**: Comment - `// 리팩토링된 코드` → Should be `// リファクタリングされたコード`

### 9. `/src/components/PdfConverter.tsx`
- **Line 50**: Comment - `// PDF 파일 목록 로드` → Should be `// PDFファイルリストをロード`
- **Line 97**: Comment - `// 변환 히스토리에 추가` → Should be `// 変換履歴に追加`
- **Line 108**: Comment - `// 실패 히스토리에 추가` → Should be `// 失敗履歴に追加`

### 10. `/src/components/AspCliWebTerminal.tsx`
- **Line 42**: Comment - `// 시스템 시간 업데이트` → Should be `// システム時間更新`
- **Line 54**: Comment - `// 터미널 스크롤 자동 조정` → Should be `// ターミナルスクロール自動調整`
- **Line 61**: Comment - `// ASP 명령어 실행` → Should be `// ASPコマンド実行`
- **Line 70**: Comment - `// Python aspcli.py 호출` → Should be `// Python aspcli.py呼び出し`
- **Line 87**: String - `'명령이 실행되었습니다.'` → Should be `'コマンドが実行されました。'`
- **Line 90**: Comment - `// 백엔드가 없는 경우 시뮬레이션` → Should be `// バックエンドがない場合のシミュレーション`
- **Line 106**: Comment - `// 에러 발생 시 시뮬레이션으로 폴백` → Should be `// エラー発生時シミュレーションにフォールバック`
- **Line 122**: Comment - `// 커서 위치 설정` → Should be `// カーソル位置設定`
- **Line 128**: Comment - `// 명령어 시뮬레이션 (백엔드 없을 때)` → Should be `// コマンドシミュレーション（バックエンドなし時）`
- **Line 132**: Comment - `// 명령어 파싱` → Should be `// コマンドパーシング`
- **Line 136**: Comment - `// 시뮬레이션 지연` → Should be `// シミュレーション遅延`
- **Line 547**: Comment - `// 커서 포커스 관리` → Should be `// カーソルフォーカス管理`
- **Line 549**: Comment - `// SMED 맵 출력에서 입력 필드가 있는지 확인` → Should be `// SMEDマップ出力で入力フィールドがあるか確認`
- **Line 550**: String check - `'입력필드'` → Should be `'入力フィールド'`
- **Line 551**: Comment - `// SMED 맵 입력 필드가 있으면 해당 필드에 포커스` → Should be `// SMEDマップ入力フィールドがあれば該当フィールドにフォーカス`
- **Line 559**: Comment - `// 그 외의 경우는 명령어 입력창에 포커스` → Should be `// その他の場合はコマンド入力欄にフォーカス`
- **Line 568**: Comment - `// 명령어 자동완성` → Should be `// コマンド自動補完`
- **Line 577**: Comment - `// 이전 명령어 불러오기 (최대 10개)` → Should be `// 前のコマンドを読み込む（最大10個）`
- **Line 588**: Comment - `// 다음 명령어 불러오기` → Should be `// 次のコマンドを読み込む`
- **Line 612**: Comment - `// 헤더` → Should be `// ヘッダー`
- **Line 634**: Comment - `// 터미널 본문` → Should be `// ターミナル本文`
- **Line 636**: Comment - `// 시작 메시지` → Should be `// 開始メッセージ`
- **Line 652**: Comment - `// 명령어 히스토리` → Should be `// コマンド履歴`
- **Line 668**: Comment - `// 실행 중 표시` → Should be `// 実行中表示`
- **Line 678**: Comment - `// 명령어 입력` → Should be `// コマンド入力`
- **Line 701**: Comment - `// 도움말 패널` → Should be `// ヘルプパネル`

### 11. `/src/components/MapLink.tsx`
- **Line 70**: State message - `'노드를 추가하고 연결하여 맵과 프로그램의 관계를 정의하세요.'` → Should be `'ノードを追加して接続し、マップとプログラムの関係を定義してください。'`
- **Line 209**: Status message - `'맵 링크 데이터가 저장되었습니다.'` → Should be `'マップリンクデータが保存されました。'`
- **Line 229**: Status message - `'${type} 노드가 추가되었습니다.'` → Should be `'${type} ノードが追加されました。'`
- **Line 500**: Status message - `'연결할 대상 노드를 클릭하세요.'` → Should be `'接続する対象ノードをクリックしてください。'`
- **Line 527**: Status message - `'연결 생성이 취소되었습니다.'` → Should be `'接続作成がキャンセルされました。'`
- **Line 602**: Status message - `'연결이 생성되었습니다.'` → Should be `'接続が作成されました。'`
- **Line 604**: Status message - `'이미 연결된 노드입니다.'` → Should be `'すでに接続されたノードです。'`
- **Line 607**: Status message - `'연결 생성이 취소되었습니다.'` → Should be `'接続作成がキャンセルされました。'`
- **Line 618**: Status message - `'노드 이동이 완료되었습니다.'` → Should be `'ノード移動が完了しました。'`
- **Line 648**: Status message - `'노드가 삭제되었습니다.'` → Should be `'ノードが削除されました。'`
- **Line 653**: Status message - `'연결이 삭제되었습니다.'` → Should be `'接続が削除されました。'`
- **Line 658**: Confirm dialog - `'모든 노드와 연결을 삭제하시겠습니까?'` → Should be `'すべてのノードと接続を削除しますか？'`
- **Line 665**: Status message - `'모든 노드와 연결이 삭제되었습니다.'` → Should be `'すべてのノードと接続が削除されました。'`
- **Line 671**: Prompt - `'파라미터 이름을 입력하세요:'` → Should be `'パラメータ名を入力してください：'`
- **Line 699**: UI Text - `'MapLink - 객체 연결 관리'` → Should be `'MapLink - オブジェクト接続管理'`
- **Line 700**: UI Text - `'SMED 맵과 프로그램 간의 연결 관계를 정의하고 키 바인딩을 설정하세요'` → Should be `'SMEDマップとプログラム間の接続関係を定義し、キーバインディングを設定してください'`
- **Line 715**: Button - `'MAP 추가'` → Should be `'MAP 追加'`
- **Line 726**: Button - `'PROGRAM 추가'` → Should be `'PROGRAM 追加'`
- **Line 732**: Tooltip - `'Shift+클릭으로 노드 연결'` → Should be `'Shift+クリックでノード接続'`
- **Line 742**: Button - `'저장'` → Should be `'保存'`
- **Line 751**: Button - `'선택 삭제'` → Should be `'選択削除'`
- **Line 759**: Button - `'전체 삭제'` → Should be `'すべて削除'`
- **Line 774**: Title - `"확대 (마우스 오버)"` → Should be `"拡大（マウスオーバー）"`
- **Line 781**: Title - `"축소 (마우스 오버)"` → Should be `"縮小（マウスオーバー）"`
- **Line 807**: Status message - Same as line 527
- **Line 821**: UI Text - `'MAP 속성'` → Should be `'MAP プロパティ'`
- **Line 826**: UI Text - `'PROGRAM 속성'` → Should be `'PROGRAM プロパティ'`
- **Line 833**: Label - `'이름'` → Should be `'名前'`
- **Line 844**: Label - `'SMED 파일'` → Should be `'SMEDファイル'`
- **Line 850**: Option - `"선택하세요"` → Should be `"選択してください"`
- **Line 858**: Label - `'프로그램 경로'` → Should be `'プログラムパス'`
- **Line 864**: Option - `"Java 클래스 선택"` → Should be `"Javaクラス選択"`
- **Line 874**: Placeholder - `"com.example.Program 또는 위에서 선택"` → Should be `"com.example.Program または上から選択"`
- **Line 886**: UI Text - `'연결 속성'` → Should be `'接続プロパティ'`
- **Line 891**: Label - `'트리거 키'` → Should be `'トリガーキー'`
- **Line 915**: Label - `'파라미터'` → Should be `'パラメータ'`
- **Line 921**: Button - `'추가'` → Should be `'追加'`
- **Line 938**: Placeholder - `"필드명"` → Should be `"フィールド名"`
- **Line 951**: UI Text - `'파라미터가 없습니다'` → Should be `'パラメータがありません'`
- **Line 961**: UI Text - `'노드나 연결을 선택하여 속성을 편집하세요'` → Should be `'ノードまたは接続を選択してプロパティを編集してください'`
- **Lines 963-966**: Instructions:
  - `'노드 추가: 상단 버튼 클릭'` → `'ノード追加：上部ボタンクリック'`
  - `'노드 이동: 드래그'` → `'ノード移動：ドラッグ'`
  - `'연결 생성: Shift+클릭'` → `'接続作成：Shift+クリック'`
  - `'삭제: 선택 후 삭제 버튼'` → `'削除：選択後削除ボタン'`
- **Line 977**: UI Text - `'노드:'` → Should be `'ノード：'`
- **Line 978**: UI Text - `'연결:'` → Should be `'接続：'`
- **Line 979**: UI Text - `'확대/축소:'` → Should be `'拡大/縮小：'`
- **Line 980**: UI Text - `'활성'` → Should be `'アクティブ'`

### 12. `/src/utils/ebcdicConverter.ts`
- **Line 2**: Comment - `* EBCDIC 변환 유틸리티` → Should be `* EBCDIC変換ユーティリティ`
- **Line 3**: Comment - `* 독립적이고 안정적인 EBCDIC ↔ ASCII 변환 함수` → Should be `* 独立的で安定的なEBCDIC ↔ ASCII変換関数`
- **Line 6**: Comment - `// 기본 EBCDIC to ASCII 매핑 테이블 (US 코드페이지 기준)` → Should be `// 基本EBCDIC to ASCIIマッピングテーブル（USコードページ基準）`
- **Line 8**: Comment - `// 제어 문자 (0x00-0x1F)` → Should be `// 制御文字 (0x00-0x1F)`
- **Line 14**: Comment - `// 공백 및 특수 문자` → Should be `// 空白および特殊文字`
- **Line 15**: Comment - `0x40: 0x20, // 공백` → Should be `0x40: 0x20, // 空白`
- **Line 206**: Comment - `0xFF: 0x20  // 공백으로 매핑` → Should be `0xFF: 0x20  // 空白にマッピング`
- **Line 209**: Comment - `// SOSI 코드 상수` → Should be `// SOSIコード定数`
- **Line 211**: Comment - `SO: 0x0E, // Shift Out - 더블바이트 모드 시작` → Should be `SO: 0x0E, // Shift Out - ダブルバイトモード開始`
- **Line 212**: Comment - `SI: 0x0F  // Shift In - 싱글바이트 모드 복귀` → Should be `SI: 0x0F  // Shift In - シングルバイトモード復帰`
- **Line 215**: Comment - `// 더블바이트 문자 매핑 (예시 - 실제 코드페이지에 따라 다름)` → Should be `// ダブルバイト文字マッピング（例 - 実際のコードページによって異なる）`
- **Line 217-220**: Comments in mapping:
  - `0x4040: 0x8140, // 전각 공백` → `0x4040: 0x8140, // 全角空白`
  - `0x4241: 0x8241, // 가` → `0x4241: 0x8241, // が`
  - `0x4242: 0x8242, // 나` → `0x4242: 0x8242, // な`
  - `// ... 더 많은 매핑 추가 가능` → `// ... より多くのマッピング追加可能`

### 13. `/src/utils/encodingConverter.ts`
- All debug messages and comments containing Korean text should be converted to Japanese
- Too many instances to list individually - mainly debug messages in the conversion functions

### 14. `/src/components/ASPMapEditor.tsx`
- **Line 48**: State message - `'드래그 앤 드롭으로 필드를 추가하고, 클릭으로 선택하여 속성을 편집하세요.'` → Should be `'ドラッグアンドドロップでフィールドを追加し、クリックで選択してプロパティを編集してください。'`
- **Line 131**: Status message - `'${fieldId} 필드가 추가되었습니다.'` → Should be `'${fieldId} フィールドが追加されました。'`
- **Line 214**: Status message - `'${selectedField} 필드가 삭제되었습니다.'` → Should be `'${selectedField} フィールドが削除されました。'`
- **Line 223**: Status message - `'모든 필드가 삭제되었습니다.'` → Should be `'すべてのフィールドが削除されました。'`
- **Line 257**: Status message - `'SMED 파일 ${filename}을 불러왔습니다.'` → Should be `'SMEDファイル ${filename} をロードしました。'`
- **Line 259**: Status message - `'SMED 파일 ${filename} 불러오기 실패'` → Should be `'SMEDファイル ${filename} のロード失敗'`
- **Line 262**: Status message - `'SMED 파일 불러오기 오류:'` → Should be `'SMEDファイルロードエラー：'`
- **Line 336**: Status message - `'SMED 파일이 다운로드되었습니다.'` → Should be `'SMEDファイルがダウンロードされました。'`
- **Line 353**: UI Text - `'드래그 앤 드롭으로 필드를 추가하고 SMED 맵을 편집하세요'` → Should be `'ドラッグアンドドロップでフィールドを追加してSMEDマップを編集してください'`
- **Line 380**: Button - `'미리보기'` → Should be `'プレビュー'`
- **Line 388**: Option - `"SMED 불러오기"` → Should be `"SMEDをロード"`
- **Line 399**: Button - `'SMED 내보내기'` → Should be `'SMEDエクスポート'`
- **Line 407**: Button - `'전체삭제'` → Should be `'すべて削除'`
- **Line 480**: UI Text - `'필드 속성'` → Should be `'フィールドプロパティ'`
- **Line 484-516**: Labels:
  - `'필드 ID'` → `'フィールドID'`
  - `'필드명'` → `'フィールド名'`
  - `'폭'` → `'幅'`
  - `'높이'` → `'高さ'`
- **Line 529**: Label - `'기본값'` → Should be `'デフォルト値'`
- **Line 539**: Label - `'속성'` → Should be `'属性'`
- **Line 545-548**: Options:
  - `"일반"` → `"一般"`
  - `"읽기전용"` → `"読み取り専用"`
  - `"필수입력"` → `"必須入力"`
  - `"비활성화"` → `"無効化"`

## Excluded Files
- `/src/i18n/ko.json` - This is the Korean language support file and should NOT be modified
- `/src/i18n/index.ts` - Line 60 contains `{ code: 'ko', name: '한국어', flag: '🇰🇷' }` which is the language selector entry for Korean and should remain as is

## Recommendations
1. All Korean text in comments should be converted to Japanese
2. All UI text, alerts, and status messages should be converted to Japanese
3. Console log messages should be converted to Japanese
4. The Korean language option in the language selector should remain unchanged as it's part of the i18n system