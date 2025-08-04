/**
 * COBOL Call Tree Analyzer
 * COBOLプログラム間のCALL関係を分析してツリー構造を生成する
 */

export interface CallInfo {
  callerProgram: string;
  calleeProgram: string;
  lineNumber?: number;
  callStatement: string;
}

export interface ProgramNode {
  name: string;
  type: 'COBOL' | 'CL';
  children: ProgramNode[];
  calls: CallInfo[];
  isFound: boolean; // プログラムファイルが存在するかどうか
  cyclic?: boolean; // 循環参照の場合
}

export interface CallTreeResult {
  rootNodes: ProgramNode[];
  allCalls: CallInfo[];
  missingPrograms: string[];
  cyclicReferences: string[][];
}

export class CobolCallTreeAnalyzer {
  private programs: Map<string, string> = new Map(); // プログラム名 -> ソースコード
  private callCache: Map<string, CallInfo[]> = new Map(); // キャッシュ

  /**
   * プログラムファイルを登録
   */
  addProgram(name: string, sourceCode: string, type: 'COBOL' | 'CL' = 'COBOL'): void {
    const programName = this.normalizeProgramName(name);
    this.programs.set(programName, sourceCode);
    console.log(`Added program: ${programName} (${sourceCode.length} chars)`);
  }

  /**
   * プログラム名を正規化（拡張子除去、大文字変換）
   */
  normalizeProgramName(name: string): string {
    return name
      .replace(/\.(cob|cobol|cpy|copy|cl|cle)$/i, '')
      .toUpperCase()
      .trim();
  }

  /**
   * COBOLソースコードからCALL文を抽出
   */
  private extractCallStatements(sourceCode: string): CallInfo[] {
    const calls: CallInfo[] = [];
    const lines = sourceCode.split('\n');
    
    // COBOL CALL文のパターン
    const callPatterns = [
      // CALL 'PROGRAM-NAME'
      /CALL\s+['"]([A-Z0-9\-_]+)['"](?:\s+USING.*?)?(?:\s+RETURNING.*?)?/gi,
      // CALL PROGRAM-NAME
      /CALL\s+([A-Z0-9\-_]+)(?:\s+USING.*?)?(?:\s+RETURNING.*?)?/gi,
      // CALL variable-name (動的CALL - プログラム名が変数の場合)
      /CALL\s+([A-Z0-9\-_]+)\s*$/gi
    ];

    lines.forEach((line, index) => {
      const trimmedLine = line.trim();
      
      // コメント行をスキップ
      if (trimmedLine.startsWith('*') || trimmedLine.startsWith('//')) {
        return;
      }

      callPatterns.forEach(pattern => {
        let match;
        while ((match = pattern.exec(trimmedLine)) !== null) {
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
  private extractCLCallStatements(sourceCode: string): CallInfo[] {
    const calls: CallInfo[] = [];
    const lines = sourceCode.split('\n');
    
    // CL CALL文のパターン
    const callPatterns = [
      // CALL PGM(PROGRAM-NAME)
      /CALL\s+PGM\s*\(\s*([A-Z0-9\-_]+)\s*\)/gi,
      // CALL 'PROGRAM-NAME'
      /CALL\s+['"]([A-Z0-9\-_]+)['"](?:\s+PARM.*?)?/gi,
      // CALL PROGRAM-NAME.LIBRARY (extract program name before dot)
      /CALL\s+([A-Z0-9\-_X]+)\.([A-Z0-9\-_]+)(?:\s|$)/gi,
      // CALL PROGRAM-NAME
      /CALL\s+([A-Z0-9\-_X]+)(?:\s+PARM.*?)?(?:\s|$)/gi
    ];

    lines.forEach((line, index) => {
      const trimmedLine = line.trim();
      
      // コメント行をスキップ
      if (trimmedLine.startsWith('/*') || trimmedLine.startsWith('//')) {
        return;
      }

      callPatterns.forEach(pattern => {
        let match;
        while ((match = pattern.exec(trimmedLine)) !== null) {
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
  private isSystemFunction(name: string): boolean {
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
  private getCallsForProgram(programName: string): CallInfo[] {
    if (this.callCache.has(programName)) {
      return this.callCache.get(programName)!;
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
  private createProgramNode(programName: string, visitedPrograms: Set<string> = new Set()): ProgramNode {
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

    const children: ProgramNode[] = [];
    
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
  private getLanguageType(programName: string): 'COBOL' | 'CL' {
    const sourceCode = this.programs.get(programName);
    if (!sourceCode) return 'COBOL';
    
    const isCL = sourceCode.includes('PGM ') || /CALL\s+PGM\s*\(/i.test(sourceCode);
    return isCL ? 'CL' : 'COBOL';
  }

  /**
   * 全プログラムのCALL関係を分析してツリー構造を生成
   */
  analyzeCallTree(): CallTreeResult {
    const allCalls: CallInfo[] = [];
    const missingPrograms: Set<string> = new Set();
    const cyclicReferences: string[][] = [];

    // 全プログラムからCALL情報を抽出
    Array.from(this.programs.keys()).forEach(programName => {
      const calls = this.getCallsForProgram(programName);
      allCalls.push(...calls);

      // 存在しないプログラムをチェック
      calls.forEach(call => {
        if (!this.programs.has(call.calleeProgram)) {
          missingPrograms.add(call.calleeProgram);
        }
      });
    });

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
  printCallTree(result: CallTreeResult): string {
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
  private printNode(node: ProgramNode, depth: number): string {
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
  printDebugInfo(): void {
    console.log('=== Debug Info ===');
    console.log(`Registered Programs: ${this.programs.size}`);
    
    Array.from(this.programs.entries()).forEach(([name, code]) => {
      const calls = this.getCallsForProgram(name);
      console.log(`${name}: ${calls.length} calls, ${code.length} chars`);
      calls.forEach(call => {
        console.log(`  -> ${call.calleeProgram} (line: ${call.lineNumber})`);
      });
    });
  }

  /**
   * キャッシュクリア
   */
  clearCache(): void {
    this.callCache.clear();
  }

  /**
   * 全データをクリア
   */
  clear(): void {
    this.programs.clear();
    this.callCache.clear();
  }
}