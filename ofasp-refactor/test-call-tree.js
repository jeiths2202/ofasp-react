/**
 * COBOL Call Tree Analyzer Unit Test
 */

const CobolCallTreeAnalyzer = require('./cobol-call-tree-analyzer.js');

// テストデータ
const testPrograms = {
  'ASPAC0X': `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASPAC0X.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           CALL 'ASPAX01X'.
           CALL 'ASPAX02X'.
           CALL 'ASPAX03X'.
           STOP RUN.
  `,
  
  'ASPAX01X': `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASPAX01X.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           CALL 'ASPAX11X'.
           CALL 'ASPAX12X'.
           EXIT PROGRAM.
  `,
  
  'ASPAX02X': `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASPAX02X.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           CALL 'ASPAX21X'.
           CALL 'COMMON01'.
           EXIT PROGRAM.
  `,
  
  'ASPAX03X': `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASPAX03X.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           CALL 'COMMON01'.
           CALL 'ASPAX31X'.
           EXIT PROGRAM.
  `,
  
  'ASPAX11X': `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASPAX11X.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY 'ASPAX11X EXECUTED'.
           EXIT PROGRAM.
  `,
  
  'COMMON01': `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMMON01.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY 'COMMON UTILITY'.
           EXIT PROGRAM.
  `,

  'CL-TEST': `
PGM
    DCL VAR(&RESULT) TYPE(*CHAR) LEN(10)
    CALL PGM(ASPAC0X)
    CALL 'ASPAX01X'
    CALL COMMON01 PARM(&RESULT)
ENDPGM
  `
};

// テスト実行
function runTests() {
  console.log('🧪 Starting COBOL Call Tree Analyzer Tests\n');
  
  try {
    const analyzer = new CobolCallTreeAnalyzer();
    
    // テスト1: プログラム登録
    console.log('📝 Test 1: Program Registration');
    Object.entries(testPrograms).forEach(([name, code]) => {
      const type = name === 'CL-TEST' ? 'CL' : 'COBOL';
      analyzer.addProgram(name, code, type);
    });
    console.log('✅ All programs registered successfully\n');
    
    // テスト2: デバッグ情報出力
    console.log('📊 Test 2: Debug Information');
    analyzer.printDebugInfo();
    console.log('');
    
    // テスト3: CALL Tree 分析
    console.log('🌳 Test 3: Call Tree Analysis');
    const result = analyzer.analyzeCallTree();
    
    console.log(`Root nodes: ${result.rootNodes.length}`);
    console.log(`Total calls: ${result.allCalls.length}`);
    console.log(`Missing programs: ${result.missingPrograms.length}`);
    
    if (result.missingPrograms.length > 0) {
      console.log('Missing programs:', result.missingPrograms);
    }
    console.log('');
    
    // テスト4: ツリー構造出力
    console.log('📋 Test 4: Tree Structure Output');
    const treeOutput = analyzer.printCallTree(result);
    console.log(treeOutput);
    
    // テスト5: 個別CALL情報確認
    console.log('🔍 Test 5: Individual Call Information');
    result.allCalls.forEach(call => {
      const status = result.missingPrograms.includes(call.calleeProgram) ? '❌' : '✅';
      console.log(`${status} ${call.callerProgram} -> ${call.calleeProgram} (line: ${call.lineNumber})`);
    });
    console.log('');
    
    // テスト6: 期待値検証
    console.log('✨ Test 6: Expected Results Validation');
    
    // ASPAC0Xが3つのプログラムを呼び出すことを確認
    const aspac0xCalls = result.allCalls.filter(call => call.callerProgram === 'ASPAC0X');
    console.log(`ASPAC0X calls: ${aspac0xCalls.length} (expected: 3)`);
    console.assert(aspac0xCalls.length === 3, 'ASPAC0X should call 3 programs');
    
    // COMMON01が複数のプログラムから呼ばれることを確認
    const common01Callers = result.allCalls.filter(call => call.calleeProgram === 'COMMON01');
    console.log(`COMMON01 called by: ${common01Callers.length} programs (expected: 3)`);
    console.assert(common01Callers.length === 3, 'COMMON01 should be called by 3 programs');
    
    // ルートプログラムの確認
    const rootNames = result.rootNodes.map(node => node.name);
    console.log(`Root programs: ${rootNames.join(', ')}`);
    console.assert(rootNames.includes('ASPAC0X'), 'ASPAC0X should be a root program');
    console.assert(rootNames.includes('CL-TEST'), 'CL-TEST should be a root program');
    
    console.log('✅ All tests passed successfully!');
    
    return result;
    
  } catch (error) {
    console.error('❌ Test failed:', error);
    console.error(error.stack);
    process.exit(1);
  }
}

// メイン実行
if (require.main === module) {
  runTests();
}

module.exports = { runTests };