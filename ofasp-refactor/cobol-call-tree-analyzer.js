/**
 * COBOL Call Tree Analyzer (JavaScript版)
 * COBOLプログラム間のCALL関係を分析してツリー構造を生成する
 */

class CobolCallTreeAnalyzer {
  constructor() {
    this.programs = new Map(); // プログラム名 -> ソースコード
    this.callCache = new Map(); // キャッシュ
  }

  /**
   * プログラムファイルを登録
   */
  addProgram(name, sourceCode, type = 'COBOL') {
    const programName = this.normalizeProgramName(name);
    this.programs.set(programName, sourceCode);
    console.log(`Added program: ${programName} (${sourceCode.length} chars)`);
  }

  /**
   * プログラム名を正規化（拡張子除去、大文字変換）
   */
  normalizeProgramName(name) {
    return name
      .replace(/\.(cob|cobol|cpy|copy|cl|cle)$/i, '')
      .toUpperCase()
      .trim();
  }

  /**
   * COBOLソースコードからCALL文を抽出
   */
  extractCallStatements(sourceCode) {
    const calls = [];
    const lines = sourceCode.split('\n');
    
    // COBOL CALL文のパターン
    const callPatterns = [
      // CALL 'PROGRAM-NAME'
      /CALL\s+['"]([A-Z0-9\-_]+)['"](?:\s+USING.*?)?(?:\s+RETURNING.*?)?/gi,
      // CALL PROGRAM-NAME
      /CALL\s+([A-Z0-9\-_]+)(?:\s+USING.*?)?(?:\s+RETURNING.*?)?/gi
    ];

    lines.forEach((line, index) => {
      const trimmedLine = line.trim();
      
      // コメント行をスキップ
      if (trimmedLine.startsWith('*') || trimmedLine.startsWith('//')) {
        return;
      }

      callPatterns.forEach(pattern => {
        let match;
        const regex = new RegExp(pattern.source, pattern.flags);
        while ((match = regex.exec(trimmedLine)) !== null) {
          const calleeName = match[1].replace(/['"]/g, '').toUpperCase();
          
          // システム関数や予約語をスキップ
          if (!this.isSystemFunction(calleeName)) {
            calls.push({
              callerProgram: '', // 後で設定
              calleeProgram: calleeName,
              lineNumber: index + 1,
              callStatement: trimmedLine
            });
          }
        }
      });
    });

    return calls;
  }

  /**
   * CLソースコードからCALL文を抽出
   */
  extractCLCallStatements(sourceCode) {
    const calls = [];
    const lines = sourceCode.split('\n');
    
    // CL CALL文のパターン
    const callPatterns = [
      // CALL PGM(PROGRAM-NAME) - 最優先
      /CALL\s+PGM\s*\(\s*([A-Z0-9\-_]+)\s*\)/gi,
      // CALL 'PROGRAM-NAME'
      /CALL\s+['"]([A-Z0-9\-_X]+)['"](?:\s+PARM.*?)?/gi
    ];

    lines.forEach((line, index) => {
      const trimmedLine = line.trim();
      
      // コメント行をスキップ
      if (trimmedLine.startsWith('/*') || trimmedLine.startsWith('//')) {
        return;
      }

      callPatterns.forEach(pattern => {
        let match;
        const regex = new RegExp(pattern.source, pattern.flags);
        while ((match = regex.exec(trimmedLine)) !== null) {
          const calleeName = match[1].replace(/['"]/g, '').toUpperCase();
          
          if (!this.isSystemFunction(calleeName)) {
            calls.push({
              callerProgram: '', // 後で設定
              calleeProgram: calleeName,
              lineNumber: index + 1,
              callStatement: trimmedLine
            });
          }
        }
      });
    });

    return calls;
  }

  /**
   * システム関数かどうかをチェック
   */
  isSystemFunction(name) {
    const systemFunctions = [
      'LENGTH', 'SUBSTR', 'INSPECT', 'STRING', 'UNSTRING',
      'ACCEPT', 'DISPLAY', 'MOVE', 'ADD', 'SUBTRACT',
      'MULTIPLY', 'DIVIDE', 'COMPUTE', 'IF', 'ELSE',
      'END-IF', 'PERFORM', 'EXIT', 'STOP', 'GOBACK'
    ];
    return systemFunctions.includes(name);
  }

  /**
   * 特定プログラムのCALL情報を取得（キャッシュ付き）
   */
  getCallsForProgram(programName) {
    if (this.callCache.has(programName)) {
      return this.callCache.get(programName);
    }

    const sourceCode = this.programs.get(programName);
    if (!sourceCode) {
      return [];
    }

    // COBOLかCLかを判定
    const isCL = sourceCode.includes('PGM ') || /CALL\s+PGM\s*\(/i.test(sourceCode);
    const calls = isCL 
      ? this.extractCLCallStatements(sourceCode)
      : this.extractCallStatements(sourceCode);

    // caller情報を設定
    calls.forEach(call => {
      call.callerProgram = programName;
    });

    this.callCache.set(programName, calls);
    return calls;
  }

  /**
   * プログラムノードを生成
   */
  createProgramNode(programName, visitedPrograms = new Set()) {
    const calls = this.getCallsForProgram(programName);
    const isFound = this.programs.has(programName);
    
    // 循環参照チェック
    if (visitedPrograms.has(programName)) {
      return {
        name: programName,
        type: this.getLanguageType(programName),
        children: [],
        calls: calls,
        isFound: isFound,
        cyclic: true
      };
    }

    const newVisited = new Set(visitedPrograms);
    newVisited.add(programName);

    const children = [];
    
    calls.forEach(call => {
      const childNode = this.createProgramNode(call.calleeProgram, newVisited);
      children.push(childNode);
    });

    return {
      name: programName,
      type: this.getLanguageType(programName),
      children: children,
      calls: calls,
      isFound: isFound
    };
  }

  /**
   * プログラムの言語タイプを判定
   */
  getLanguageType(programName) {
    const sourceCode = this.programs.get(programName);
    if (!sourceCode) return 'COBOL';
    
    const isCL = sourceCode.includes('PGM ') || /CALL\s+PGM\s*\(/i.test(sourceCode);
    return isCL ? 'CL' : 'COBOL';
  }

  /**
   * 全プログラムのCALL関係を分析してツリー構造を生成
   */
  analyzeCallTree() {
    const allCalls = [];
    const missingPrograms = new Set();
    const cyclicReferences = [];

    // 全プログラムからCALL情報を抽出
    for (const [programName] of this.programs) {
      const calls = this.getCallsForProgram(programName);
      allCalls.push(...calls);

      // 存在しないプログラムをチェック
      calls.forEach(call => {
        if (!this.programs.has(call.calleeProgram)) {
          missingPrograms.add(call.calleeProgram);
        }
      });
    }

    // ルートノード（他のプログラムから呼ばれていないプログラム）を特定
    const calledPrograms = new Set(allCalls.map(call => call.calleeProgram));
    const rootPrograms = Array.from(this.programs.keys())
      .filter(program => !calledPrograms.has(program));

    // ルートノードがない場合（すべて相互参照）は最初のプログラムをルートとする
    if (rootPrograms.length === 0 && this.programs.size > 0) {
      rootPrograms.push(Array.from(this.programs.keys())[0]);
    }

    // 各ルートからツリーを生成
    const rootNodes = rootPrograms.map(rootProgram => 
      this.createProgramNode(rootProgram)
    );

    return {
      rootNodes,
      allCalls,
      missingPrograms: Array.from(missingPrograms),
      cyclicReferences
    };
  }

  /**
   * ツリー構造をテキスト形式で出力
   */
  printCallTree(result) {
    let output = '=== COBOL Call Tree Analysis ===\n\n';
    
    output += `Total Programs: ${this.programs.size}\n`;
    output += `Total Calls: ${result.allCalls.length}\n`;
    output += `Missing Programs: ${result.missingPrograms.length}\n`;
    output += `Root Programs: ${result.rootNodes.length}\n\n`;

    if (result.missingPrograms.length > 0) {
      output += '❌ Missing Programs:\n';
      result.missingPrograms.forEach(program => {
        output += `  - ${program}\n`;
      });
      output += '\n';
    }

    output += '🌳 Call Tree Structure:\n';
    result.rootNodes.forEach(rootNode => {
      output += this.printNode(rootNode, 0);
    });

    return output;
  }

  /**
   * ノードを再帰的に出力
   */
  printNode(node, depth) {
    const indent = '  '.repeat(depth);
    const icon = node.isFound ? '📄' : '❌';
    const cyclic = node.cyclic ? ' (CYCLIC)' : '';
    const type = node.type === 'CL' ? '[CL]' : '[COBOL]';
    
    let output = `${indent}${icon} ${node.name} ${type}${cyclic}\n`;
    
    if (node.calls.length > 0 && depth < 10) { // 深度制限
      node.children.forEach(child => {
        output += this.printNode(child, depth + 1);
      });
    }
    
    return output;
  }

  /**
   * デバッグ情報出力
   */
  printDebugInfo() {
    console.log('=== Debug Info ===');
    console.log(`Registered Programs: ${this.programs.size}`);
    
    for (const [name, code] of this.programs) {
      const calls = this.getCallsForProgram(name);
      console.log(`${name}: ${calls.length} calls, ${code.length} chars`);
      calls.forEach(call => {
        console.log(`  -> ${call.calleeProgram} (line: ${call.lineNumber})`);
      });
    }
  }

  /**
   * キャッシュクリア
   */
  clearCache() {
    this.callCache.clear();
  }

  /**
   * 全データをクリア
   */
  clear() {
    this.programs.clear();
    this.callCache.clear();
  }
}

module.exports = CobolCallTreeAnalyzer;